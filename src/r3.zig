//! A trace logging facility.
//!
//! The intent of the package is to replace the classic level-based
//! debug logging. Sprinkle trace log events tastefully around your
//! code. Typically, all trace log events are silent. However, you can
//! enable events selectively to log events even in production
//! environments.
//!
//! Usage:
//!
//! ```
//! const r3 = @import("r3");
//! const TRACE = r3.trace;
//! const TRACE_ENABLED = r3.enabled;
//!
//! ...
//!     try r3.select("^MYAPP-", null);
//! ...
//!     TRACE("MYAPP-AWAIT-CONNECT UID={}", .{self.uid});
//! ```
//!
//! Notes:
//! * Using all-upper-case "TRACE" makes it easier for the eye to
//!   separate trace events from the overall form of the source code.
//! * While the use of "TRACE" is analogous to `std.fmt.format`, the
//!   intent is to stick to a rigorous output syntax for easier
//!   postprocessing.
//! * The first word of the format string (here:
//!   "MYAPP-AWAIT-CONNECT") is the name of the event. Typically, each
//!   event bears a unique name but that is not mandatory. The name is
//!   used by `select` to enable and disable events.
//! * The package depends on libc's regular expression API.

const std = @import("std");
const re = @cImport(@cInclude("regex.h"));

/// The "hidden" structure of a trace log event. Events get created at
/// comptime in a "dormant" state (`.state` = `.unlinked`). As the program
/// execution runs into TRACE statements, the corresponding events are
/// added to the global trace_events collection and linked is set to
/// true. At the same time, the name of the event is matched against
/// the current trace log selection and enabled is set accordingly.
///
/// Conversely, whenever the event selection rule changes, all events
/// in the trace_events collection are traversed and their enabled
/// fields are updated.
const Event = struct {
    const State = enum { unlinked, disabled, enabled };

    name: []const u8,
    state: State = .unlinked,
};

// Any dynamic collection type will do.
const EventList = std.SinglyLinkedList(Event);

/// Add the event node to `trace_events` and check it against the
/// selection criterion if it hasn't yet been added. Return true iff
/// the associated event should produce log output.
fn eventNodeEnabled(node: *EventList.Node) bool {
    const event = &node.data;
    if (event.state == .disabled) {
        // A fast path, which might give us a wrong `false` outcome.
        // That's a risk worth taking.
        return false;
    }
    // A slow path properly synchronized.
    if (@cmpxchgStrong(
        Event.State,
        &event.state,
        .unlinked,
        .disabled,
        .monotonic,
        .monotonic,
    )) |state| {
        // The event was linked (or being linked).
        return state == .enabled;
    }
    // The event was unlinked. We are responsible for adding the event
    // to `trace_events`. In the interim, other callers will see it as
    // disabled.
    if (global_mutex.tryLock()) {
        defer global_mutex.unlock();
        trace_events.prepend(node);
        var work_area: [1000]u8 = undefined;
        filterEvent(&work_area, event);
        return event.state == .enabled;
    }
    // We didn't get the lock. We can't block as we might be in the
    // middle of a signal handler, which could result in a deadlock.
    // In case of contention, we simply restore the event to the
    // unlinked state and treat it as disabled.
    @atomicStore(Event.State, &event.state, .unlinked, .monotonic);
    return false;
}

/// The list of "known" trace log events. Events get added to the list
/// lazily as the program execution runs into `trace` (TRACE)
/// statements.
var trace_events = EventList{};

/// Any access to trace_events or patterns must take place while
/// holding `global_mutex`.
var global_mutex: std.Thread.Mutex = .{};

// Bitfields are used in libc's regex_t definition. Zig can't cope
// with them yet so we resort to this hack, reserving a sufficiently
// large blob of bytes for the purpose.
const fake_regex_t = [512]u8; // Currently, 64 would suffice for Linux.

fn asRegex(pattern: *fake_regex_t) *re.regex_t {
    return @ptrCast(pattern);
}

/// The trace event selection is based on a pair of optional regular
/// expressions. Each trace log event is enabled or disabled as follows:
/// * If `.include` is null, the event is disabled.
/// * Otherwise, if `.include`.? fails to match its name, the event is
///   disabled.
/// * Otherwise, if `.exclude` is null, the event is enabled.
/// * Otherwise, if `.exclude`.? matches its name, the event is
///   disabled.
/// * Otherwise, the event is enabled.
var patterns: struct {
    include: ?fake_regex_t = null,
    exclude: ?fake_regex_t = null,
} = .{};

pub const ReError = error{
    /// The given string splice is too large for the preallocated work
    /// area.
    Overflow,

    /// There is a syntax error in the regular expression.
    BadRegex,
};

// Convert a byte slice to a NUL-terminated C string.
fn zeroTerminate(
    dest: []u8,
    orig: []const u8,
) ReError![:0]const u8 {
    if (orig.len >= dest.len)
        return error.Overflow;
    std.mem.copyForwards(u8, dest, orig);
    dest[orig.len] = '\x00';
    return dest[0..orig.len :0];
}

// A convenience wrapper for libc's regcomp(3) function.
fn compileRegularExpression(
    pattern: *re.regex_t,
    work_area: []u8,
    regex: []const u8,
) ReError!void {
    if (re.regcomp(pattern, try zeroTerminate(work_area, regex), 0) != 0)
        return error.BadRegex;
}

// A convenience wrapper for libc's regexec(3) function.
fn matchRegularExpression(
    pattern: *const re.regex_t,
    work_area: []u8,
    string: []const u8,
) ReError!bool {
    const string0 = try zeroTerminate(work_area, string);
    var matches: [1]re.regmatch_t = undefined;
    return re.regexec(pattern, string0, 1, &matches, 0) == 0;
}

// Enable or disable an event based on the current selection rule.
// Must be called while holding `global_mutex`.
fn filterEvent(work_area: []u8, event: *Event) void {
    var state: Event.State = .disabled;
    defer @atomicStore(Event.State, &event.state, state, .monotonic);
    if (patterns.include) |*include| {
        if (matchRegularExpression(
            asRegex(include),
            work_area,
            event.name,
        ) catch return) {
            if (patterns.exclude) |*exclude| {
                if (matchRegularExpression(
                    asRegex(exclude),
                    work_area,
                    event.name,
                ) catch return) {
                    return;
                }
            }
            state = .enabled;
            return;
        }
    }
}

/// Specify the trace log event selection rule using two optional
/// regular expressions. Events whose names match `include_regex` but
/// do not match `exclude_regex` are enabled; others are disabled. If
/// `include_regex` is null, all events are disabled. If
/// `exclude_regex` is null, only `include_regex` is considered.
///
/// Do not call `select` from a signal handler as that could result in
/// a deadlock.
pub fn select(include_regex: ?[]const u8, exclude_regex: ?[]const u8) !void {
    global_mutex.lock();
    defer global_mutex.unlock();
    var work_area: [1000]u8 = undefined;
    if (patterns.include) |*include| {
        re.regfree(asRegex(include));
        patterns.include = null;
    }
    if (patterns.exclude) |*exclude| {
        re.regfree(asRegex(exclude));
        patterns.exclude = null;
    }
    const dummy_fake_regex_t: fake_regex_t = undefined;
    if (include_regex) |regex| {
        patterns.include = dummy_fake_regex_t;
        compileRegularExpression(
            asRegex(&patterns.include.?),
            &work_area,
            regex,
        ) catch |err| {
            patterns.include = null;
            return err;
        };
    }
    if (exclude_regex) |regex| {
        patterns.exclude = dummy_fake_regex_t;
        compileRegularExpression(
            asRegex(&patterns.exclude.?),
            &work_area,
            regex,
        ) catch |err| {
            patterns.exclude = null;
            return err;
        };
    }
    var node_maybe = trace_events.first;
    while (node_maybe) |node| : (node_maybe = node.next) {
        filterEvent(&work_area, &node.data);
    }
}

/// Return true iff the named trace log event is enabled. For visual
/// reasons, the function is conventionally given the alias
/// "TRACE_ENABLED". The function is used if evaluation the arguments
/// to a "TRACE" call are elaborate and time-consuming, e.g.:
///
/// ```
///     if (TRACE_ENABLED("MYAPP-DB-SIZE")) {
///         const n = db_total_count(db);
///         TRACE("MYAPP-DB-SIZE COUNT={}", .{n});
///     }
/// ```
///
/// Simple references to variables, fields or array elements do not
/// incur a performance penalty as the compiler can optimize them out
/// if the event is disabled.
pub fn enabled(comptime event_name: []const u8) bool {

    // Each TRACE or TRACE_ENABLED statement produces a globally
    // unique EventList node.
    const Singleton = struct {
        var node = EventList.Node{ .data = .{ .name = event_name } };
    };

    return eventNodeEnabled(&Singleton.node);
}

/// Conditionally emit a trace log event. For visual reasons, the
/// function is conventionally given the alias "TRACE". Even though
/// the arguments permit a fully arbitrary log output, the intent is
/// that a rigorous format should be used:
///
/// ```
///    identifier { ' ' key '=' value }
/// ```
///
/// where
/// * "identifier" and "key" should consist of capital letters, digits
///   and dashes ('-') and
/// * "value" should contain only printable (7-bit) ASCII excluding
///   ' '.
///
/// Format wrappers like `hex` and `str` help produce compliant output
/// out of data blobs and strings.
///
/// Each trace log line is ended with a newline so it should not be
/// part of the given format string.
pub fn trace(comptime fmt: []const u8, args: anytype) void {
    const sep_loc = comptime std.mem.indexOfScalar(u8, fmt, ' ');
    const event_name = fmt[0..(sep_loc orelse fmt.len)];
    if (enabled(event_name))
        log(fmt, args);
}

/// Unconditionally produce a trace log entry. Should only be used
/// very sparingly if at all.
pub fn log(comptime fmt: []const u8, args: anytype) void {
    trace_writer.print(fmt ++ "\n", args) catch return;
}

/// A convenience type template for the creation of writer types.
pub fn UniversalWriter(comptime Context: type) type {
    return std.io.GenericWriter(Context, anyerror, Context.write);
}

/// Construct a writer object out of a context object. The context object
/// must have a write method.
pub fn makeWriter(context: anytype) UniversalWriter(@TypeOf(context)) {
    return .{ .context = context };
}

const RelativeDate = struct {
    month: u4, // range: 1..12
    day: u5, // range: 1..31
    year_offset: u1,
};

// Date math works best when the year begins on March 1. Leap years
// need no special treatment.
const year_day_dates: [366]RelativeDate = init: {
    const month_break = // starting with March
        [_]i11{ 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337, 366 };
    var at_month: u4 = 0; // indexes month_break
    var month: u4 = 3;
    var offset_days: i10 = -1;
    var dates: [366]RelativeDate = undefined;
    var year_day: i10 = 0; // March 1
    while (year_day < 366) : (year_day += 1) {
        if (year_day == month_break[at_month]) {
            month += 1;
            offset_days = year_day - 1;
            at_month += 1;
        }
        dates[year_day] = .{
            .month = (month - 1) % 12 + 1,
            .day = year_day - offset_days,
            .year_offset = month / 13,
        };
    }
    break :init dates;
};

const Date = struct { year: u16, month: u4, day: u5 };

const MemoizedDate = struct {
    // The low 32-bits encode "days_since_epoch" as-is. The high 32
    // bits encode the corresponding date.
    var memoized: u64 = 1970 << 48 | 1 << 40 | 1 << 32;

    inline fn get(days_since_epoch: u20) ?Date {
        const value = @atomicLoad(u64, &MemoizedDate.memoized, .unordered);
        if (days_since_epoch != value & 0xfffff)
            return null;
        return .{
            .year = @intCast(value >> 48),
            .month = @intCast(value >> 40 & 0xff),
            .day = @intCast(value >> 32 & 0xff),
        };
    }

    inline fn set(days_since_epoch: u20, date: Date) Date {
        const year: u64 = date.year;
        const month: u64 = date.month;
        const day: u64 = date.day;
        const value = year << 48 | month << 40 | day << 32 | days_since_epoch;
        @atomicStore(u64, &MemoizedDate.memoized, value, .unordered);
        return date;
    }
};

inline fn epoch_days_to_date(days_since_epoch: u20) Date {
    // The same date is likely to be used exclusively for 24 hours at
    // a time. No point recalculating it every time.
    if (MemoizedDate.get(days_since_epoch)) |date| {
        return date;
    }
    // Gregorian cycle is 400 years. Rebase the epoch to 1600-03-01
    // for simpler leap-year logic.
    const year_of_beginning: u16 = 1600;
    // number of days between 1600-03-01 and 1970-01-01
    const days_from_beginning_till_epoch: u20 = 135080;
    const days_per_gregorian_cycle = 146097;
    const days_per_leap_cycle = 1461;
    var days_remaining = days_from_beginning_till_epoch + days_since_epoch;
    const gregorian_cycles_since_beginning =
        days_remaining / days_per_gregorian_cycle;
    days_remaining %= days_per_gregorian_cycle;
    const centuries_within_gregorian_cycle =
        (days_remaining * 4 + 3) / days_per_gregorian_cycle;
    days_remaining -=
        centuries_within_gregorian_cycle * days_per_gregorian_cycle / 4;
    const leap_cycles_within_century =
        days_remaining / days_per_leap_cycle;
    days_remaining %= days_per_leap_cycle;
    const years_within_leap_cycle =
        (days_remaining * 4 + 3) / days_per_leap_cycle;
    days_remaining -= years_within_leap_cycle * days_per_leap_cycle / 4;
    const relative_date = &year_day_dates[days_remaining];
    return MemoizedDate.set(
        days_since_epoch,
        .{
            .year = @intCast(year_of_beginning +
                gregorian_cycles_since_beginning * 400 +
                centuries_within_gregorian_cycle * 100 +
                leap_cycles_within_century * 4 +
                years_within_leap_cycle +
                relative_date.year_offset),
            .month = relative_date.month,
            .day = relative_date.day,
        },
    );
}

const DateAndTime = struct {
    year: u16,
    month: u4,
    day: u5,
    hour: u5,
    minute: u6,
    second: u6,
    microsecond: u30,

    /// Print an epoch timestamp using the given writer in this format:
    ///
    ///     YYYY-MM-DD hh:mm:ss.ffffff
    ///
    /// UTC is used but the timezone ("Z") is not appended.
    pub fn format(
        self: DateAndTime,
        _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print(
            "{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}.{d:0>6}",
            .{
                self.year,
                self.month,
                self.day,
                self.hour,
                self.minute,
                self.second,
                self.microsecond,
            },
        );
    }
};

/// Convert the epoch timestamp into a UTC date and time.
fn microTimestampToDateAndTime(stamp_us: u60) DateAndTime {
    const us: u30 = @intCast(stamp_us % 1_000_000);
    const stamp_s: u17 = @intCast(stamp_us % 86_400_000_000 / 1_000_000);
    const s: u6 = @intCast(stamp_s % 60);
    const stamp_min: u11 = @intCast(stamp_s / 60);
    const min: u6 = @intCast(stamp_min % 60);
    const h: u5 = @intCast(stamp_min / 60);
    const date = epoch_days_to_date(@intCast(stamp_us / 86_400_000_000));
    return .{
        .year = date.year,
        .month = date.month,
        .day = date.day,
        .hour = h,
        .minute = min,
        .second = s,
        .microsecond = us,
    };
}

/// A wrapper facility to produce ISO 8601 UTC timestamps at the
/// beginning of each line.
pub fn UTCTimeStamper(comptime UnderlyingWriter: type) type {
    // Two types are produced:
    // * `UTCTimeStamper`
    // * `UTCTimeStamper.Engine`
    //
    // The former type acts as a small handle that can be copied
    // around and passed by value. The latter type represents the
    // time stamping engine itself. The caller must allocate,
    // initialize and maintain the engine.
    return struct {
        engine: *Engine,

        const Self = @This();

        fn write(self: Self, bytes: []const u8) !usize {
            return try self.engine.write(bytes);
        }

        const Engine = struct {
            underlying_writer: UnderlyingWriter,
            at_beginning_of_line: bool = true,

            fn write(self: *Engine, bytes: []const u8) !usize {
                for (bytes) |byte| {
                    if (self.at_beginning_of_line) {
                        const stamp_us: u60 =
                            @intCast(std.time.microTimestamp());
                        try self.underlying_writer.print(
                            "{} ",
                            .{microTimestampToDateAndTime(stamp_us)},
                        );
                    }
                    try self.underlying_writer.writeByte(byte);
                    self.at_beginning_of_line = byte == '\n';
                }
                return bytes.len;
            }
        };
    };
}

// Construct time_stamp_writer.
const DefaultTraceWriter = struct {
    const stderr_writer = std.io.getStdErr().writer();
    const StderrTimeStamper = UTCTimeStamper(@TypeOf(stderr_writer));
    var time_stamp_engine = StderrTimeStamper.Engine{
        .underlying_writer = stderr_writer,
    };
    const time_stamp_writer = makeWriter(
        StderrTimeStamper{ .engine = &time_stamp_engine },
    );
};

/// This global variable decides where trace output goes. By default,
/// the output goes to the standard error (with an ISO 8601 UTC
/// timestamp at the beginning of each line), which is normally not a
/// good idea for services as their trace log output can easily
/// inundate the system journal. Rather, the user should plug in a
/// writer that produces trace output to a dedicated area, with
/// attention given to log rotation.
///
/// You can replace the default trace writer like this:
///
///     r3.trace_writer = my_writer.any();
///
/// For example, to print the log to the standard output with no
/// timestamps, do this:
///
///     r3.trace_writer = std.io.getStdOut().writer().any();
pub var trace_writer = DefaultTraceWriter.time_stamp_writer.any();

const Hex = struct {
    byte_string: []const u8,

    pub fn format(
        self: Hex,
        _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.byte_string) |byte| {
            try writer.print("{x:0>2}", .{byte});
        }
    }
};

/// Produce a hexadecimal encoding of a byte string. Example:
///
/// ```
///     TRACE("MYAPP-READ UID={} DATA={}", .{ self.uid, r3.hex(data[0..16]) });
/// ```
pub fn hex(byte_string: []const u8) Hex {
    return Hex{ .byte_string = byte_string };
}

const Str = struct {
    chars: []const u8,

    pub fn format(
        self: Str,
        _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.chars) |c| {
            switch (c) {
                ' ' => {
                    try writer.print("+", .{});
                },
                '/', '-', '.', '_', '~' => {
                    try writer.print("{c}", .{c});
                },
                else => {
                    if (std.ascii.isAlphanumeric(c)) {
                        try writer.print("{c}", .{c});
                    } else {
                        try writer.print("%{x:0>2}", .{c});
                    }
                },
            }
        }
    }
};

/// Produce a printable encoding of an arbitrary character string. Example:
///
/// ```
///     TRACE("MYAPP-READ UID={} DATA={}", .{ self.uid, r3.str(self.name) });
/// ```
///
/// A form of URL ("percent") encoding is used. The space (' ')
/// character is encoded as a plus ('+') sign. The output is 7-bit
/// ASCII; UTF-8 produces hard-to-read output.
pub fn str(chars: []const u8) Str {
    return Str{ .chars = chars };
}

const Ptr = struct {
    obj: *const anyopaque,

    pub fn format(
        self: Ptr,
        _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{x}", .{@intFromPtr(self.obj)});
    }
};

/// Produce a printable encoding of an arbitrary pointer. Example:
///
/// ```
///     TRACE("MYAPP-READ UID={} PTR={}", .{ self.uid, r3.ptr(self) });
/// ```
///
/// A hexadecimal memory address is produced.
pub fn ptr(object: *const anyopaque) Ptr {
    return Ptr{ .obj = object };
}

/// An integer type returned by `newUID`.
pub const UID = u32;

/// A convenience function to generate (rather) unique object
/// identifiers. It is customary to assign each object a UID and
/// include it on every trace log event pertaining to the object. That
/// helps target textual filtering to the generated log.
pub fn newUID() UID {
    // A fixed-seed pseudorandom number generator can be beneficial
    // for the reproducibility of trace logs.
    const UIDGenerator = struct {
        var prng = std.rand.DefaultPrng.init(0);
        const rand = prng.random();
    };

    return UIDGenerator.rand.int(UID);
}

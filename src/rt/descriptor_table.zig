// Optimized hash table designed for quick int -> int lookups. Stores keys and values together,
// prioritizing minimizing cache misses at the cost of potentially wasted memory from
// alignment. Implements robin hood hashing and a fast reduction function (instead of modulo).
// The table is unmanaged - it accepts an allocator as an argument to insert instead of storing
// it to minimize memory footprint. The maximum number of elements is 2^32.
const std = @import("std");

const Hasher = std.hash.Wyhash;
const asBytes = std.mem.asBytes;

// TODO: performance ideas:
// * blindly searching for small tables
// * rapidhash instead of wyhash
// * replace PSL with accessor hotness as a swap metric
// * simd lookup for small tables
// * avoid wrap around during probe

pub fn DescriptorTable(comptime Key: type, comptime Value: type, comptime empty: Value) type {
    return struct {
        ptr: [*]Entry = &.{},
        count: u32 = 0,
        capacity: u32 = 0,

        const Entry = struct {
            key: Key,
            value: Value,
        };

        pub const Table = @This();

        pub fn get(table: *const Table, key: Key) ?Value {
            const hash = Hasher.hash(0, asBytes(&key));
            var index = reduce(hash, table.capacity);

            var psl = 0;
            while (true) : (psl += 1) {
                const entry = table.ptr[index + psl];
                // if the slot is empty, the entry wasn't found
                if (entry.value == empty) return null;
                // otherwise if the key matches, we've found the entry
                if (entry.key == key) return entry.value;
                // increment index, and wrap around if needed
                if (index == table.capacity) {
                    index = 0;
                } else {
                    index += 1;
                }
            }
        }

        inline fn reduce(hash: u64, len: u32) u32 {
            return @truncate((hash * len) >> 32);
        }
    };
}

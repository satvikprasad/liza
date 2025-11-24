# [Liza](https://www.lizalang.com)
A custom interpreter for Liza written in OCaml.

## Examples
Liza is pretty cool. It's a dynamically typed language, and so far it supports arrays, recursion, and closures. You can actually write an elementary merge sort in Liza:

```
var unsorted = [
    "satvik" 
    "andrew" 
    "ronnie" 
    "banana"
];

// Capture stdlib functions: len, append
var merge = fn (a b) [len append] {
    var i = 0;
    var j = 0;

    var result = [];

    while i < len(a) or j < len(b) {
        if i >= len(a) {
            result = append (result b[j]);
            j = j + 1;

        } else if j >= len(b) {
            result = append (result a[i]);
            i = i + 1;

        } else if a[i] < b[j] {
            result = append (result a[i]);
            i = i + 1;
        } else {
            result = append (result b[j]);
            j = j + 1;
        }
    }

    ret result;
};

// Capture stdlib functions: floor
var merge_sort = fn (list start end)[floor] {
    if end == start {
        ret [list[start]];
    }

    if end < start {
        ret [];
    }

    var mid = floor ((start + end) / 2);

    var left = merge_sort (list start mid);
    var right = merge_sort (list mid + 1 end);

    ret merge (left right);
};

print "Sorted";
print unsorted;
print "INTO";
print (merge_sort (unsorted 0 len(unsorted) - 1));
```


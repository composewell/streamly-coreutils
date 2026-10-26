# tree traversals

Orders in which a directory tree can be visited, the streamly mechanism
for each, and the `Traversal` constructor of `Streamly.Coreutils.Find`
that uses it.

Status: survey; terminology not settled.

## Serial traversals

### Example tree

The outputs below are for this tree, with the entries of each directory
returned by the operating system in the order shown. 5, 7, 8 and 9 are
files.

```
1
|-- 2
|   |-- 4
|   |   `-- 8
|   `-- 5
`-- 3
    |-- 6
    |   `-- 9
    `-- 7
```

### Axes

A serial traversal is described by three independent choices:

* Emit (Order of parent vs children): A directory is emitted before its
  entries (pre-order), or after its whole subtree (post-order). In
  post-order a file is emitted when it is read.
* Descend (direction):
  * deep: descend into a subdirectory as soon as it is read, keeping the
    parent directory open;
  * wide: finish reading a directory, then descend into each of its
    subdirectories;
  * level: finish every directory at one depth before starting the next
    depth.
* Siblings (Order of siblings): Subdirectories are expanded in the order
  they were read (forward), or the last read first (reverse).

### Table

"fds" is the number of open directory handles. Streamly names are from
streamly-core 0.4 (streamly master) unless noted.

| Emit | Descend | Siblings | Output | Resources | Streamly | Find |
|---|---|---|---|---|---|---|
| pre | narrow | forward | 1 2 4 8 5 3 6 9 7 | fds = depth | `concatIterate`, `StreamK.concatIterateWith append`, `unfoldIterateMaybe` | `DepthFirst` |
| pre | narrow | reverse | - | - | - | - |
| pre | wide | forward | 1 2 3 4 5 8 6 7 9 | 1 fd; unread subdirectories along the path in memory | `wideDfsConcatIterate`, `wideDfsUnfoldIterate` | planned |
| pre | wide | reverse | 1 2 3 6 7 9 4 5 8 | same | `wideDfsConcatIterateRev` (`altBfsConcatIterate` in 0.3), `wideDfsUnfoldIterateRev` | `WideDepthFirstReverse` |
| pre | level | forward | 1 2 3 4 5 6 7 8 9 | 1 fd; one level in memory | `bfsConcatIterate`, `bfsUnfoldIterateMaybe` | `BreadthFirst` |
| pre | level | reverse | 1 2 3 6 7 4 5 8 9 | same | `bfsConcatIterateRev`, `bfsUnfoldIterateRev` | planned |
| post | narrow | forward | 8 4 5 2 9 6 7 3 1 | fds = depth | none | planned (`DepthFirstPostOrder`) |
| post | narrow | reverse | - | - | - | - |
| post | wide | forward | 5 8 4 2 7 9 6 3 1 | 1 fd; unread subdirectories and unfinished directories along the path in memory | none | - |
| post | wide | reverse | 7 9 6 3 5 8 4 2 1 | same | none | - |
| post | level | forward | 5 7 8 9 4 6 2 3 1 | 1 fd; every directory path in memory | none | - |
| post | level | reverse | - | same | none | - |

The pre-order outputs were produced by running the streamly
combinators. The post-order outputs are from a pure model of each rule;
the same model reproduces the pre-order outputs.

### Notes

* Narrow with reverse siblings does not exist. A narrow traversal
  descends before the directory has been read to the end, so the last
  entry is not known yet. Reading the directory to the end first makes
  the traversal wide.
* Narrow post-order is the order of GNU `find -depth`: entries in the
  order read, each subdirectory replaced by its subtree followed by the
  subdirectory itself.
* Wide post-order also emits every directory after its contents, which
  is the order `rm -r` needs, and keeps 1 fd open. It emits the files of
  a directory before descending into its subdirectories, so the order
  differs from GNU `find -depth`.
* Post-order with level descent keeps every directory path in memory
  until the deepest level has been read. The reverse variant is left
  undefined.
* Level with reverse siblings: `bfsConcatIterateRev` expands each level
  in the reverse of the order in which the previous level was emitted.
  Relative to the order the directories were read, the levels alternate:
  depth 2 is reversed (6 7 4 5), depth 3 is forward (8 9).
* The `*UnfoldIterate*` combinators are the fused versions of the concat
  ones and give the same order. In streamly-core 0.3 they open every
  pending directory as soon as it is emitted, so a level traversal can
  exceed the open file limit. On master they store the seeds and open a
  directory only when it is expanded; not yet tested on a directory tree.

### Traversals outside the axes

* Iterative deepening: a narrow pre-order traversal repeated with depth
  limits 1, 2, 3, and so on. The output has the pre / level / forward
  order, with fds = depth and no level held in memory, but a directory at
  depth d is read once for each depth limit greater than d. No streamly
  combinator; it can be built from `concatIterate` and a depth limit.
* Interleaving: `StreamK.mergeIterateWith interleave` (the dropped
  `Interleaved` constructor) and `StreamK.concatIterateWith interleave`.
  Neither has a defined emit point or descent rule; both mix entries from
  several open directories.

## Parallel traversals

### Example tree

The outputs below are for the same tree as the serial traversals, with
the entries of each directory returned in the order shown. 5, 7, 8 and 9
are files.

```
1
|-- 2
|   |-- 4
|   |   `-- 8
|   `-- 5
`-- 3
    |-- 6
    |   `-- 9
    `-- 7
```

### Mechanism

Streamly has one concurrent tree iteration, `parConcatIterate cfg`. A
worker emits an entry, puts the rest of that directory's stream on the
channel's work queue, and continues into the subtree of the entry. A
free worker takes the next stream from the work queue. The `cfg`
selects the queue:

* default: a LIFO queue (`enqueueLIFO` in `Channel/Append.hs`);
* `interleaved True`: a FIFO queue (`enqueueFIFO` in
  `Channel/Interleave.hs`);
* `ordered True`: a LIFO queue, with the output reordered into the
  serial order (`enqueueAhead` in `Channel/Append.hs`).

The comment "The channel q should be FIFO for DFS, otherwise it is BFS"
in `parConcatIterate` has FIFO and LIFO the other way round: with one
worker the LIFO queue gives the pre / narrow / forward order.

### Axes

A parallel traversal is described by three independent choices:

* Emit: a directory is emitted before its entries (pre-order), or after
  its whole subtree (post-order), as in the serial traversals.
* Schedule: the work a free worker takes next:
  * newest: the stream queued last (LIFO);
  * oldest: the stream queued first (FIFO);
  * level: all directories at one depth are read concurrently, and the
    next depth starts when all of them have been read.
* Output: unordered, each entry emitted as soon as it is read; or
  ordered, the entries emitted in the order of a serial traversal while
  the directories are read concurrently.

### Table

For an unordered traversal the output column gives the order with one
worker (`maxThreads 1`). For an ordered traversal it gives the order of
the output, for any number of workers. "fds" is the number of open
directory handles; the parConcatIterate entries are derived from the
mechanism (every queued stream is a partly read directory), not
measured.

| Emit | Schedule | Output | Output order | Resources | Streamly | Find |
|---|---|---|---|---|---|---|
| pre | newest | unordered | 1 2 4 8 5 3 6 9 7 | fds = partly read directories, along the paths of all workers | `parConcatIterate id` | `DepthFirstParallel` |
| pre | oldest | unordered | 1 2 3 4 6 5 8 7 9 | fds = partly read directories, all open directories in round robin | `parConcatIterate (interleaved True)` | `BreadthFirstParallel` (misnamed) |
| pre | level | unordered | 1 2 3 4 5 6 7 8 9 | fds = workers; one level in memory | none; a level loop with `parConcatMap` over each level | - |
| pre | newest | ordered as pre / narrow / forward | 1 2 4 8 5 3 6 9 7 | as pre / newest, plus the output read ahead of the consumer | `parConcatIterate (ordered True)` | `DepthFirstParallelOrdered` |
| pre | - | ordered as pre / wide / forward | 1 2 3 4 5 8 6 7 9 | - | none | - |
| pre | - | ordered as pre / wide / reverse | 1 2 3 6 7 9 4 5 8 | - | none | - |
| pre | level | ordered as pre / level / forward | 1 2 3 4 5 6 7 8 9 | fds = workers; one level in memory, plus the output read ahead | none; a level loop with `parConcatMap (ordered True)` over each level | planned (`BreadthFirstParallelOrdered`) |
| post | newest | unordered | 8 4 5 2 9 6 7 3 1 | - | none; needs a count of unfinished entries per directory | - |
| post | oldest | unordered | - | - | none | - |
| post | level | unordered | 5 7 8 9 4 6 2 3 1 | every directory path in memory | none | - |
| post | - | ordered as post / narrow / forward | 8 4 5 2 9 6 7 3 1 | - | none | - |

The outputs of the three `parConcatIterate` rows were produced by
running them on the example tree. The outputs of the other rows are the
intended orders, equal to the serial order with the same emit, descent
and siblings.

### Notes

* The oldest schedule is not a level traversal. With one worker on a
  tree in which directory 2 has a chain 4 -> 5 -> 6 -> 7 and directory 3
  has the files 31 to 36, it emits 1 2 3 4 31 5 32 6 33 7 34 35 36: one
  entry from each open directory in turn. That order equals
  `StreamK.concatIterateWith interleave` on the same tree. The level
  traversal would emit 31 to 36 before 5, 6 and 7.
* The newest schedule with one worker gives the pre / narrow / forward
  order exactly.
* Running the three `parConcatIterate` modes with several workers on the
  example tree gave the same outputs as one worker. The tree is too small
  for the workers to overlap, so this does not show the orders under real
  concurrency.
* For an ordered traversal the schedule affects only which directories
  are read ahead of the output, not the output order.
* Unordered post-order is the order a parallel `rm -r` or `du` needs: a
  directory is emitted after all its entries, but the entries of
  different directories may be emitted in any order.

### Naming

Proposed rule: name an ordered parallel traversal by its output order,
and an unordered one by the serial order it gives with one worker. Under
this rule:

* `DepthFirstParallel` (newest, unordered) keeps its name: one worker
  gives the pre / narrow / forward order.
* `BreadthFirstParallel` (oldest, unordered) is renamed, e.g. to
  `InterleavedParallel` or `RoundRobinParallel`: one worker gives an
  interleaved order, not a level order.
* `BreadthFirstParallel` is used for the pre / level / unordered
  traversal.

### Traversals outside the axes

* The other `Config` options of `parConcatIterate` (`maxThreads`,
  `maxBuffer`, `eager`, rate) change performance, not order. They belong
  in a separate modifier, not in `Traversal`.
* Work stealing: each worker keeps its own LIFO queue and an idle worker
  takes the oldest stream from another worker's queue. It gives the same
  guarantee as the pre / newest / unordered traversal, with less
  contention on a shared queue. It would be a new channel type.
* `parMergeIterate`, a concurrent `StreamK.mergeIterateWith interleave`,
  exists in streamly but is unimplemented (`undefined`).

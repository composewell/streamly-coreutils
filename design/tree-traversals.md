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

* Emit: a directory is emitted before its entries (pre-order), or after
  its whole subtree (post-order). In post-order a file is emitted when it
  is read.
* Descend:
  * narrow: descend into a subdirectory as soon as it is read, keeping the
    parent directory open;
  * wide: finish reading a directory, then descend into each of its
    subdirectories;
  * level: finish every directory at one depth before starting the next
    depth.
* Siblings: subdirectories are expanded in the order they were read
  (forward), or the last read first (reverse).

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

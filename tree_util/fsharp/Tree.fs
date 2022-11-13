module Tree

let enumEntries path =
    System.IO.Directory.GetFileSystemEntries path
    |> Array.toList


type Summary =
    {
        Name: string
        IsDir: bool
        IsHidden: bool
        IsLink: bool
        Size: int64
        Children: Summary list
        ParentDir: string
    }

let private addTrailingSepChar (str: string) : string =
    let sepChar =
        System.IO.Path.DirectorySeparatorChar.ToString()

    if str.EndsWith sepChar then
        str
    else
        str + sepChar

let mutable calls = 0

let rec toSummary path =
    calls <- calls + 1
    printfn $"CALLS: %d{calls}"
    let fi = System.IO.FileInfo path

    let isDir =
        fi.Attributes.HasFlag(System.IO.FileAttributes.Directory)

    let isHidden =
        fi.Attributes.HasFlag(System.IO.FileAttributes.Hidden)

    let isLink = fi.LinkTarget = null |> not

    // Return 0 or get a runtime error (data does not exist).
    let size =
        if isDir then int64 0 else fi.Length

    let children =
        if isDir && not isLink then
            (enumEntries path |> List.collect toSummary)
        else
            []

    [
        {
            Name = path
            IsDir = isDir
            IsHidden = isHidden
            IsLink = isLink
            Size = size
            Children = children
            ParentDir = fi.DirectoryName |> addTrailingSepChar
        }
    ]

let rec summaryListToData (depth: int) (summaryList: Summary list) : (int * int64 * string * bool * string) list =
    summaryList
    |> List.filter (fun s -> not s.IsHidden) // mimic os util: "tree"
    |> List.collect (fun summary ->
        let children: Summary list = summary.Children

        if not (List.isEmpty children) then
            [
                (depth, summary.Size, summary.Name, summary.IsDir, summary.ParentDir)
            ]
            |> List.append (summaryListToData (depth + 1) children |> List.map id)

        else
            [
            (depth, summary.Size, summary.Name, summary.IsDir, summary.ParentDir)
            ])

let hiddenFile summary = summary.IsHidden

let toLowerName summary = summary.Name.ToLower()

let pathToSummaryList path =
    enumEntries path |> List.collect toSummary


let private toHuman size =
    ByteSizeLib
        .ByteSize
        .FromBytes(float size)
        .ToString()


let private printPath (depth, size: int64, path: string, isDir, parentDir) =
    let depthToWS = String.replicate depth "  "

    let simplePath = path.Replace(parentDir, "")

    let fileTypeEmoji =
        if isDir then "📁" else "📝"

    let depth_ = $"%d{depth}".PadLeft(3)
    printfn $"[%s{(toHuman size).PadLeft(9)}] %s{fileTypeEmoji} [%s{depth_}] %s{depthToWS} %s{simplePath}"

let print rootPath =
    pathToSummaryList (rootPath |> addTrailingSepChar)
    |> summaryListToData 0
    |> List.sortBy (fun (depth, _size, path, _isDir, _parentDir) -> [ path.ToLower(), depth ]) // sort lower to mimic os util "tree"
    |> List.iter printPath

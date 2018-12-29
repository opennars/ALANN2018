module CommandProcessor

open Types
open CommandParser
open FileIO
open TermFormatters
open Akka.Streams.Dsl
open System.IO
open System
open ALANNSystem
open System.Collections.Concurrent

let getNodeFromTerm term =
    let numStreams = Params.NUM_TERM_STREAMS 
    let hashRoute numStreams t = abs(t.GetHashCode() % numStreams )
    let i = hashRoute numStreams term

    stores.[i].TryGetValue(term)

let showGeneralBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        myprintfn (sprintf "#COMMAND: SHOW_GENERAL_BELIEFS FOR TERM '%s'" (ft term))
        let beliefs = [for b in node.Beliefs.GetGeneralBeliefs() -> b]
        beliefs
        |> Seq.iter (fun b ->myprintfn (sprintf "!%s" (formatBelief b)))
        beliefs
        |> Seq.averageBy (fun b -> b.TV.C)
        |> (fun avg -> myprintfn (sprintf "!AVERAGE CONF = %f" avg))
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" (ft term))

let showTemporalBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        myprintfn (sprintf "#COMMAND: SHOW_TEMPORAL_BELIEFS FOR TERM '%s'" (ft term))
        let beliefs = [for b in node.Beliefs.GetTemporalBeliefs() -> b]
        beliefs
        |> Seq.iter (fun b ->myprintfn (sprintf "!%s" (formatBelief b)))
        beliefs
        |> Seq.averageBy (fun b -> b.TV.C)
        |> (fun avg -> myprintfn (sprintf "!AVERAGE CONF = %f" avg))
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" (ft term))

let showNode term =
    match getNodeFromTerm term with
    | (true, node) ->
        myprintfn (sprintf "#COMMAND: SHOW_NODE FOR TERM '%s'" (ft term))
        myprintfn (sprintf "#ATTENTION = [%f] TERM = %s" node.Attention (ft node.Term))
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" (ft term))

let getNodeCount() =
    let rec loop acc n =
        match n with
        | 0 -> acc + stores.[n].Count
        | _ -> loop (acc + stores.[n].Count) (n - 1)

    loop 0 (stores.GetLength(0) - 1)
    
let nodeCount() =
    myprintfn "#COMMAND: NODE_COUNT"
    myprintfn (sprintf "#TOTAL NODE COUNT: %d" (getNodeCount()))

let enableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        myprintfn (sprintf "#COMMAND: ENABLE TRACE FOR TERM '%s'" (ft term))
        node.Trace <- true
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** UNENABLE TO SET TRACE FOR TERM '%s' ***" (ft term))

let disableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        myprintfn (sprintf "#COMMAND: DISABLE TRACE FOR TERM '%s'" (ft term))
        node.Trace <- false
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" (ft term))

let pauseFlow() =
    myprintfn "#COMMAND: PAUSE"
    myprintfn "#FLOW PAUSED - USE CONTINUE (C) TO CONTINUE FLOW"
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Close) |> ignore

let continueFlow() =
    myprintfn "#COMMAND: CONTINUE"
    myprintfn "#FLOW CONTINUED - USE PAUSE (P) TO PAUSE FLOW"
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Open) |> ignore

let loadFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    match File.Exists filepath with
    | true -> 
        myprintfn (sprintf "#COMMAND: LOAD FILE '%s'"filepath)
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.LoadGraph filepath
        myprintfn "#FILE LOADED"
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** FILE '%s' DOES NOT EXIST ***" filepath)

let saveFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    createDirectoryIfNotExists Params.STORAGE_PATH

    match File.Exists filepath with
    | false -> 
        myprintfn (sprintf "#COMMAND: SAVE FILE '%s'" filepath)
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.ExportGraph filepath
        myprintfn "#FILE SAVED"
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> myprintfn (sprintf "#COMMAND ERROR: *** FILE '%s' EXISTS ***" filepath)

let reset() =
    myprintfn "#COMMAND: RESET IN PROGRESS"
    resetSwitch <- true
    System.Threading.Thread.Sleep(1000)
    stores <- [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.MINOR_BLOCK_SIZE)|]

    cycle := 0L
    startTime <- DateTime.Now.Ticks
    resetSwitch <- false
    myprintfn "#COMMAND: RESET COMPLETE"

let processCommand (cmd : string) =
    let cmd' = cmd.TrimStart([|'#'|])
    match commandParser cmd' with
    | Show_General_Beliefs(term) -> showGeneralBeliefs term
    | Show_Temporal_Beliefs(term) -> showTemporalBeliefs term
    | Show_Node(term) -> showNode(term)
    | Node_Count -> nodeCount()
    | Enable_Trace(term) -> enableTrace term
    | Disable_Trace(term) -> disableTrace term
    | Pause -> pauseFlow()
    | Continue -> continueFlow()
    | Load(file) -> loadFile(file)
    | Save(file) -> saveFile(file)
    | Reset -> reset()
    | _ -> ()
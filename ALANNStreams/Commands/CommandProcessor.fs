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
        myprintfn (sprintf "%sCOMMAND: SHOW_GENERAL_BELIEFS FOR TERM '%s'" Params.COMMAND_PREFIX (ft term))
        let beliefs = [for b in node.Beliefs.GetGeneralBeliefs() -> b]
        beliefs
        |> Seq.iter (fun b ->myprintfn (sprintf "%s%s" Params.BELIEF_PREFIX (formatBelief b)))
        beliefs
        |> Seq.averageBy (fun b -> b.TV.C)
        |> (fun avg -> myprintfn (sprintf "%sAVERAGE CONF = %f" Params.BELIEF_PREFIX avg))
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" Params.COMMAND_PREFIX (ft term))

let showTemporalBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        myprintfn (sprintf "%sCOMMAND: SHOW_TEMPORAL_BELIEFS FOR TERM '%s'" Params.COMMAND_PREFIX (ft term))
        let beliefs = [for b in node.Beliefs.GetTemporalBeliefs() -> b]
        beliefs
        |> Seq.iter (fun b ->myprintfn (sprintf "%s%s" Params.BELIEF_PREFIX (formatBelief b)))
        beliefs
        |> Seq.averageBy (fun b -> b.TV.C)
        |> (fun avg -> myprintfn (sprintf "%sAVERAGE CONF = %f" Params.BELIEF_PREFIX avg))
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" Params.COMMAND_PREFIX (ft term))

let showNode term =
    match getNodeFromTerm term with
    | (true, node) ->
        myprintfn (sprintf "%sCOMMAND: SHOW_NODE FOR TERM '%s'" Params.COMMAND_PREFIX (ft term))
        myprintfn (sprintf "%sATTENTION = [%f] TERM = %s" Params.COMMAND_PREFIX node.Attention (ft node.Term))
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" Params.COMMAND_PREFIX (ft term))

let getNodeCount() =
    let rec loop acc n =
        match n with
        | 0 -> acc + stores.[n].Count
        | _ -> loop (acc + stores.[n].Count) (n - 1)

    loop 0 (stores.GetLength(0) - 1)
    
let nodeCount() =
    myprintfn (sprintf "%sCOMMAND: NODE_COUNT" Params.COMMAND_PREFIX)
    myprintfn (sprintf "%sTOTAL NODE COUNT: %d" Params.COMMAND_PREFIX (getNodeCount()))

let enableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        myprintfn (sprintf "%sCOMMAND: ENABLE TRACE FOR TERM '%s'" Params.COMMAND_PREFIX (ft term))
        node.Trace <- true
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** UNENABLE TO SET TRACE FOR TERM '%s' ***" Params.COMMAND_PREFIX (ft term))

let disableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        myprintfn (sprintf "%sCOMMAND: DISABLE TRACE FOR TERM '%s'" Params.COMMAND_PREFIX (ft term))
        node.Trace <- false
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** TERM '%s' DOES NOT EXIST ***" Params.COMMAND_PREFIX (ft term))

let pauseFlow() =
    myprintfn (sprintf "%sCOMMAND: PAUSE"Params.COMMAND_PREFIX)
    myprintfn (sprintf "%sFLOW PAUSED - USE CONTINUE (C) TO CONTINUE FLOW" Params.COMMAND_PREFIX)
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Close) |> ignore

let continueFlow() =
    myprintfn (sprintf "%sCOMMAND: CONTINUE" Params.COMMAND_PREFIX)
    myprintfn (sprintf "%sFLOW CONTINUED - USE PAUSE (P) TO PAUSE FLOW" Params.COMMAND_PREFIX)
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Open) |> ignore

let loadFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    match File.Exists filepath with
    | true -> 
        myprintfn (sprintf "%sCOMMAND: LOAD FILE '%s'" Params.COMMAND_PREFIX filepath)
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.LoadGraph filepath
        myprintfn (sprintf "%sFILE LOADED" Params.COMMAND_PREFIX)
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** FILE '%s' DOES NOT EXIST ***" Params.COMMAND_PREFIX filepath)

let saveFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    createDirectoryIfNotExists Params.STORAGE_PATH

    match File.Exists filepath with
    | false -> 
        myprintfn (sprintf "%sCOMMAND: SAVE FILE '%s'" Params.COMMAND_PREFIX filepath)
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.ExportGraph filepath
        myprintfn (sprintf "%sFILE SAVED" Params.COMMAND_PREFIX)
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> myprintfn (sprintf "%sCOMMAND ERROR: *** FILE '%s' EXISTS ***" Params.COMMAND_PREFIX filepath)

let reset() =
    myprintfn (sprintf "%sCOMMAND: RESET IN PROGRESS" Params.COMMAND_PREFIX)
    resetSwitch <- true
    System.Threading.Thread.Sleep(1000)
    stores <- [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.MINOR_BLOCK_SIZE)|]

    cycle := 0L
    startTime <- DateTime.Now.Ticks
    resetSwitch <- false
    myprintfn (sprintf "%sCOMMAND: RESET COMPLETE" Params.COMMAND_PREFIX)

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
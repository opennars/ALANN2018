module NodeStore

open C5
open Types

type INodeStore =
    abstract Contains : Key -> bool
    abstract Insert : Key * Node -> unit
    abstract Update : Key * Node -> unit
    abstract TryGetValue : Key -> Node option
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetNodes : unit -> seq<Node>

type SubStore(n : int, ranking : Node -> float32) =
    let q = IntervalHeap<Node>(n) :> IPriorityQueue<Node>
    let d = HashDictionary<Term, IPriorityQueueHandle<Node>>() :>IDictionary<Term, IPriorityQueueHandle<Node>>

    let maxSize = n

    let addNode key (node : Node) h =
        q.Add(h, node) |> ignore
        d.Add(key, !h)        

    let deleteMinNode() =
        let (deleted, h) = q.DeleteMin()
        let h = ref h
        match d.Remove(deleted.Term, h ) with
        | true -> ()
        | false -> failwith "ConceptStore.Insert() : failed to remove on maxSize"    
        !h

    interface INodeStore with
        member x.Contains(key) = d.Contains key

        member x.Insert(key, node) =
            if d.Count >= maxSize then
                if ranking(node) >= ranking(q.FindMin()) then                     
                    ref <| deleteMinNode()
                    |> addNode key node
            else
                let h = ref null
                addNode key node (ref null)

        member x.Update(key, node) =

            let h = d.[key]
            q.[h] <- node

        member x.TryGetValue key =    
        
            if d.Contains(key) then Some(q.[d.[key]])
            else None

        member x.Clear() =        
            d.Clear()
            while not(q.IsEmpty) do
                let (_,_) = q.DeleteMax()
                ()

        member x.Count = d.Count
        member x.GetNodes() = q :> seq<Node>

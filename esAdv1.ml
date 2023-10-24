module type HasType = sig
    type t
end

module SocialNetwork(M: HasType) = struct
    type node = M.t * (edge list)
    and edge = relationship * M.t
    and relationship = Friendship | Kinship | FinancialExchange | Dislike
    | SexualRelationship | RelationshipOfBelifs | Knowledge | Prestige
    type graph = node list

    let rec addEdge graph (v1, e)  = match graph with
        | [] -> (v1, [e])::graph
        | (v, edges)::l when(v == v1) -> (v, e::edges)::l
        | h::l -> h::(addEdge l (v1, e))

    let edgesToGraph list =
        let rec edgesToGraph res = function
        | [] -> res
        | h::l -> edgesToGraph (addEdge res h) l
    in edgesToGraph [] list


    let visitAll graph =
        let rec neighborsToNodesList (graph: graph) = function
        | [] -> []
        | (_, nb)::l -> let node = List.find(fun (n, _) -> n == nb) graph in node::(neighborsToNodesList graph l)
        in let rec dfs (visited: edge list) = function
        | n when (List.exists (fun v -> (fst n) == (snd v)) visited) -> ()
        | n -> let nbList = neighborsToNodesList graph (snd n) in List.iter2(fun v e -> (dfs (e::visited) v)) nbList (snd n)
    in List.iter(fun n -> dfs [] n) graph

end

module IntSocialNetwork = SocialNetwork(Int)

let sn = IntSocialNetwork.edgesToGraph([1, (IntSocialNetwork.Friendship, 2) ; 2, (IntSocialNetwork.Friendship, 1)])

let () = IntSocialNetwork.visitAll sn

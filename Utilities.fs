namespace Zombies19

module Utilities =

    let rec printSocialTree tree indent =
        match tree with
        | PersonNode(person, children) ->
            printfn $"{indent}{person.Name} (Age: {person.Age}, Status: {person.Status})"

            for child in children do
                printSocialTree child (indent + "_")
        | Empty -> ()

    let memoize f =
        let cache = System.Collections.Generic.Dictionary<_, _>()

        (fun x ->
            if cache.ContainsKey(x) then
                cache[x]
            else
                let result = f x
                cache[x] <- result
                result)

    let rec findPathToOrigin (tree, origin, path) =
        match tree with
        | PersonNode(person, _) when person = origin -> Some(person :: path)
        | PersonNode(person, children) ->
            children
            |> List.map (fun child -> findPathToOrigin (child, origin, person :: path))
            |> List.tryPick id
        | Empty -> None

    let findPathToOriginMemoized = memoize findPathToOrigin

    let rec infectAncestors tree path =
        match tree with
        | PersonNode(person, children) when List.contains person path ->
            let updatedPerson = { person with Status = Status.ZombieB }
            let updatedChildren = children |> List.map (fun child -> infectAncestors child path)
            PersonNode(updatedPerson, updatedChildren)
        | PersonNode(person, children) ->
            let updatedChildren = children |> List.map (fun child -> infectAncestors child path)
            PersonNode(person, updatedChildren)
        | Empty -> Empty

    let createStatusChanger condition status =
        (fun person ->
            if condition person then
                { person with Status = status }
            else
                person)

    let rec infectZombieA treeToUpdate origin isChild =
        let statusChanger =
            createStatusChanger
                (fun person ->
                    (person = origin && person.Status <> Status.VaccinA1)
                    || (isChild && person.Status <> Status.VaccinA1))
                ZombieA

        match treeToUpdate with
        | PersonNode(person, children) ->
            let updatedChildren =
                children
                |> List.map (fun child ->
                    match child with
                    | PersonNode(childPerson, childChildren) ->
                        infectZombieA (PersonNode(childPerson, childChildren)) origin (person = origin || isChild)
                    | Empty -> Empty)

            PersonNode(statusChanger person, updatedChildren)
        | Empty -> Empty

    let rec infectZombie32 tree =
        match tree with
        | PersonNode(person, children) when person.Age >= 32 && person.Status <> Status.VaccinA1 ->
            let updatedPerson = { person with Status = Status.Zombie32 }
            let updatedChildren = List.map infectZombie32 children
            PersonNode(updatedPerson, updatedChildren)
        | PersonNode(person, children) ->
            let updatedChildren = List.map infectZombie32 children
            PersonNode(person, updatedChildren)
        | Empty -> Empty

    let rec infectZombieC treeToUpdate origin =
        match treeToUpdate with
        | PersonNode(person, children) when person = origin ->
            let updatedPerson = { person with Status = Status.ZombieC }

            let updatedChildren =
                children
                |> List.mapi (fun index child ->
                    match child with
                    | PersonNode(childPerson, childChildren) ->
                        if index % 2 = 0 then
                            PersonNode(
                                { childPerson with
                                    Status = Status.ZombieC },
                                childChildren
                            )
                        else
                            child
                    | Empty -> Empty)

            PersonNode(updatedPerson, updatedChildren)
        | PersonNode(person, children) ->
            PersonNode(person, List.map (fun child -> infectZombieC child origin) children)
        | Empty -> Empty

    let infectZombieUltimate tree =
        match tree with
        | PersonNode(person, children) when person.Status <> Status.VaccinU ->
            PersonNode({ person with Status = Status.ZombieU }, children)
        | _ -> tree

    let infectTreeFromOrigin tree origin variant =
        match variant with
        | Status.ZombieA -> infectZombieA tree origin false
        | Status.ZombieB ->
            match findPathToOriginMemoized (tree, origin, []) with
            | Some path -> infectAncestors tree path
            | None -> tree
        | Status.Zombie32 -> infectZombie32 tree
        | Status.ZombieC -> infectZombieC tree origin
        | Status.ZombieU -> infectZombieUltimate tree
        | _ -> tree

    let rec vaccinateZombieA1 tree =
        match tree with
        | PersonNode(person, children) when
            person.Age <= 30
            && (person.Status = Status.ZombieA || person.Status = Status.Zombie32)
            ->
            let updatedPerson = { person with Status = Status.VaccinA1 }
            let updatedChildren = List.map vaccinateZombieA1 children
            PersonNode(updatedPerson, updatedChildren)
        | PersonNode(person, children) ->
            let updatedChildren = List.map vaccinateZombieA1 children
            PersonNode(person, updatedChildren)
        | Empty -> Empty

    let rec vaccinateZombieB1 tree =
        let rec vaccinateHelper children index =
            match children with
            | [] -> []
            | PersonNode(person, childChildren) :: rest when
                person.Status = Status.ZombieB || person.Status = Status.ZombieC
                ->
                let updatedStatus = if index % 2 = 0 then Status.Dead else Status.VaccinB1
                let updatedPerson = { person with Status = updatedStatus }
                let updatedChildren = vaccinateHelper childChildren 0
                PersonNode(updatedPerson, updatedChildren) :: vaccinateHelper rest (index + 1)
            | child :: rest -> child :: vaccinateHelper rest index

        match tree with
        | PersonNode(person, children) ->
            let updatedChildren = vaccinateHelper children 0
            PersonNode(person, updatedChildren)
        | Empty -> Empty

    let rec vaccinateZombieU tree =
        match tree with
        | PersonNode(person, children) when person.Status = Status.ZombieU ->
            let updatedPerson = { person with Status = Status.VaccinU }
            let updatedChildren = List.map vaccinateZombieU children
            PersonNode(updatedPerson, updatedChildren)
        | PersonNode(person, children) ->
            let updatedChildren = List.map vaccinateZombieU children
            PersonNode(person, updatedChildren)
        | Empty -> Empty

    let vaccinateTree tree variant =
        match variant with
        | Status.VaccinA1 -> vaccinateZombieA1 tree
        | Status.VaccinB1 -> vaccinateZombieB1 tree
        | Status.VaccinU -> vaccinateZombieU tree
        | _ -> tree

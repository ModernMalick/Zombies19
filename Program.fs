namespace Zombies19

module Simulation = 

    open Utilities
    
    [<EntryPoint>]
    let main argv =

        let personA = { Name = "PersonA"; Age = 25; Status = Status.Sane }
        let personB = { Name = "PersonB"; Age = 30; Status = Status.Sane }
        let personC = { Name = "PersonC"; Age = 28; Status = Status.Sane }
        let personD = { Name = "PersonD"; Age = 22; Status = Status.Sane }
        let personE = { Name = "PersonE"; Age = 35; Status = Status.Sane }
        let personF = { Name = "PersonF"; Age = 27; Status = Status.Sane }
        let personG = { Name = "PersonG"; Age = 32; Status = Status.Sane }
        let personH = { Name = "PersonH"; Age = 23; Status = Status.Sane }
        let personI = { Name = "PersonI"; Age = 29; Status = Status.Sane }
        
        let groupTree =
            PersonNode(personA, [
                PersonNode(personB, [
                    PersonNode(personD, []);
                    PersonNode(personE, [PersonNode(personF, [PersonNode(personI, [])])]);
                ]);
                PersonNode(personC, [PersonNode(personG, [PersonNode(personH, [])])]);
            ])

        let infectedTree = infectTreeFromOrigin groupTree personB Status.ZombieU
        printSocialTree infectedTree ""
        
        let vaccinatedTree = vaccinateTree infectedTree Status.VaccinU
        printSocialTree vaccinatedTree ""

        let infectedTree = infectTreeFromOrigin vaccinatedTree personB Status.ZombieU
        printSocialTree infectedTree ""
        
        0
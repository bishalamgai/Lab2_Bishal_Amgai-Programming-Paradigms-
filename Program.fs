// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello! I'm Bishal Amgai(22083566)"

printfn "Instruction:1 Records"
// Define the Coach record
type Coach = {
    Name: string
    FormerPlayer: bool
}

// Define the Stats record
type Stats = {
    Wins: int
    Losses: int
}

// Define the Team record
type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

// Function to create a Coach record
let createCoach name formerPlayer =
    { Name = name; FormerPlayer = formerPlayer }

// Function to create a Stats record
let createStats wins losses =
    { Wins = wins; Losses = losses }

// Function to create a Team record
let createTeam name coach wins losses =
    { Name = name; Coach = coach; Stats = createStats wins losses }

// Create coaches
let coaches = [
    createCoach "Gregg Popovich" true
    createCoach "Erik Spoelstra" false
    createCoach "Steve Kerr" true
    createCoach "Tyronn Lue" true
    createCoach "Doc Rivers" false
]

// Create teams with stats
let teams = [
    createTeam "San Antonio Spurs" (List.item 0 coaches) 1707 1079
    createTeam "Miami Heat" (List.item 1 coaches) 1451 1339
    createTeam "Golden State Warriors" (List.item 2 coaches) 2944 2290
    createTeam "Los Angeles Clippers" (List.item 3 coaches) 1571 1888
    createTeam "Philadelphia 76ers" (List.item 4 coaches) 2833 2686
]

// Filter successful teams
let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

// Calculate success percentage of each team
let successPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0

// Map success percentage for each team
let successPercentages = successfulTeams |> List.map successPercentage

// Print success percentages
printfn "Success percentages:"
successPercentages |> List.iteri (fun i perc -> printfn "%s: %.2f%%" successfulTeams.[i].Name perc)


printfn "Instruction:2 Discriminated Union"
// Define the Cuisine discriminated union
type Cuisine =
    | Nepali
    | Bangali

// Define the MovieType discriminated union
type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

// Define the Activity discriminated union
type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

// Function to calculate the budget based on activity
let calculateBudget (activity : Activity) =
    match activity with
    | BoardGame -> 10.0
    | Chill -> 5.0
    | Movie movieType ->
        match movieType with
        | Regular -> 16.0
        | IMAX -> 20.0
        | DBOX -> 30.0
        | RegularWithSnacks -> 16.0 + 5.0
        | IMAXWithSnacks -> 20.0 + 5.0
        | DBOXWithSnacks -> 30.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Nepali -> 100.0
        | Bangali -> 80.0
    | LongDrive (kilometers, fuelChargePerKm) -> float kilometers * fuelChargePerKm

// Example usage
let activity1 = BoardGame
let activity2 = Movie RegularWithSnacks
let activity3 = Restaurant Nepali
let activity4 = LongDrive (100, 0.5)
let activity5 = Chill

printfn "Budget for Activity 1: %.2f CAD" (calculateBudget activity1)
printfn "Budget for Activity 2: %.2f CAD" (calculateBudget activity2)
printfn "Budget for Activity 3: %.2f CAD" (calculateBudget activity3)
printfn "Budget for Activity 4: %.2f CAD" (calculateBudget activity4)
printfn "Budget for Activity 5: %.2f CAD" (calculateBudget activity5)



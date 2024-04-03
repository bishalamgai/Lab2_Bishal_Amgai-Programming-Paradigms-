// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello! I'm Bishal Amgai(22083566)"

printfn "Instruction:1 Records"

type Coach = {
    Name: string
    FormerPlayer: bool
}

type Stats = {
    Wins: int
    Losses: int
}

type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

let teams = [
    { Name = "San Antonio Spurs"; Coach = { Name = "Gregg Popovich"; FormerPlayer = true }; Stats = { Wins = 1707; Losses = 1079 } }
    { Name = "Miami Heat"; Coach = { Name = "Erik Spoelstra"; FormerPlayer = false }; Stats = { Wins = 1451; Losses = 1339 } }
    { Name = "Golden State Warriors"; Coach = { Name = "Steve Kerr"; FormerPlayer = true }; Stats = { Wins = 2944; Losses = 2290 } }
    { Name = "Los Angeles Clippers"; Coach = { Name = "Tyronn Lue"; FormerPlayer = true }; Stats = { Wins = 1571; Losses = 1888 } }
    { Name = "Philadelphia 76ers"; Coach = { Name = "Doc Rivers"; FormerPlayer = false }; Stats = { Wins = 2833; Losses = 2686 } }
]

let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

let successPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0

let successPercentages = successfulTeams |> List.map successPercentage

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



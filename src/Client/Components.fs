module Components

    open Elmish.React
    open Fable.React
    open Feliz
    open Feliz.ElmishComponents
    open Fable.React.Props

    let classMap = List.filter fst >> List.map snd >> String.concat " " >> Class

    module EmployeePicker =
        open Elmish

        type State =
            { Input            :string
              Results          :string list
              Searching        :bool
              LastSearchFailed :bool }

        type Msg =
            | ChangeQuery    of string
            | SetResults     of string list
            | SetSearchError of bool
            | SelectResult   of string

        let init = { Input = ""; Results = []; Searching = false; LastSearchFailed = false }

        let render (state: State) dispatch =
            let dropdownItem value = p [ Class "dropdown-item"; OnClick (fun _ -> value |> SelectResult |> dispatch) ] [ str value ]

            let dropdownMenu items =
                div [ Class "dropdown-menu" ] [ div [ Class "dropdown-content" ] (List.map dropdownItem items) ]

            let search =
                div [ Class "dropdown-trigger" ]
                    [ div [ Class "field" ]
                          [ p [ classMap [ true, "control is-expanded has-icons-right"; state.Searching, "is-loading" ] ]
                              [ input [ Class "input"; Type "search"; Placeholder "Search..."; valueOrDefault state.Input; OnChange (fun e -> e.Value |> ChangeQuery |> dispatch) ]
                                span  [ Class "icon is-small is-right" ] [ i [ Class "fa fa-search" ] [] ] ] ] ]

            div [ classMap [ true, "dropdown"; not state.Results.IsEmpty, "is-active" ] ]
                [ yield search
                  if state.Input.Length > 0 then
                    yield dropdownMenu state.Results ]

        let update (search: string -> Async<string list>) (select: string -> unit) msg state =
            match msg with
            | ChangeQuery query ->
                { state with Input = query
                             Searching = true }, Cmd.OfAsync.either search query SetResults (fun _ -> SetSearchError true)
            | SetResults results ->
                { state with Results = results
                             Searching = false }, Cmd.none
            | SetSearchError x ->
                { state with LastSearchFailed = x
                             Searching = false }, Cmd.none
            | SelectResult result ->
                state, Cmd.OfFunc.attempt select result (fun _ -> SetSearchError true)

    let EmployeePicker (search: string -> Async<string list>) (select: string -> unit) =
        React.elmishComponent ("EmployeePicker", EmployeePicker.init, EmployeePicker.update search select, EmployeePicker.render )
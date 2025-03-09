let get_selectors config =
  Otoml.Helpers.find_strings_opt config ["selector"] |> Option.to_result ~none:{|Missing required option "selector"|}

let no_container_action selectors message =
  Logs.debug @@ fun m -> m {|Page has no elements matching selectors "%s": %s|}
    (selectors |> Utils.format_list ~quote:false) message


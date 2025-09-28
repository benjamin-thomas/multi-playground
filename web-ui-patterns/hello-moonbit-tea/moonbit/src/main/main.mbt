///|
pub struct Model {
  counter : Int
  errMsg : String
  users : Array[User]
} derive(Show)

///|
typealias @tea.Cmd

///|
fnalias @tea.none

///|
struct User {
  id : Int
  name : String
} derive(Show)

///|
enum Msg {
  Inc
  Dec
  Rst
  GetUsers
  GotUsers(Result[Array[User], String])
}

///|
typealias @h.Html

///|
fn get_users() -> Cmd[Msg] {
  @http.get(
    "https://jsonplaceholder.typicode.com/users",
    expect=Json(GotUsers(_), decode_users),
  )
}

///|
fn user_from_json(json : Json) -> User? {
  match json {
    { "id": Number(id), "name": String(name), .. } =>
      Some({ id: id.to_int(), name })
    _ => None
  }
}

///|
fn decode_users(json : Json) -> Result[Array[User], String] {
  match json {
    Array(elems) => Ok(elems.filter_map(user_from_json))
    _ => Err("Invalid users data")
  }
}

///|
fn update(msg : Msg, model : Model) -> (Cmd[Msg], Model) {
  match msg {
    Inc => (none(), { ..model, counter: model.counter + 1 })
    Dec => (none(), { ..model, counter: model.counter - 1 })
    Rst => (none(), { ..model, counter: 0 })
    GetUsers => (get_users(), model)
    GotUsers(result) =>
      (
        none(),
        match result {
          Ok(users) => { ..model, users, errMsg: "" }
          Err(err) => { ..model, errMsg: err }
        },
      )
  }
}

///|
fn view(model : Model) -> Html[Msg] {
  @h.div([
    @h.pre([@h.text(model.to_string())]),
    @h.h1([@h.text(model.counter.to_string())]),
    @h.h2([@h.text("Demo: Title A")]),
    @h.button(click=Dec, [@h.text("-")]),
    @h.span(style=["margin: 10px", "font-size: 25px"], [
      @h.text(model.counter.to_string()),
    ]),
    @h.button(click=Inc, [@h.text("+")]),
    @h.br(),
    @h.button(click=Rst, [@h.text("Reset")]),
    @h.br(),
    @h.button(click=GetUsers, [@h.text("Get Users")]),
    @h.hr(),
    @h.h2([@h.text("Users")]),
    @h.ul(model.users.map(user => @h.li([@h.text(user.name)]))),
  ])
}

///|
fn main {
  let model : Model = { counter: 0, users: [], errMsg: "" }
  @tea.startup(model~, update~, view~)
}

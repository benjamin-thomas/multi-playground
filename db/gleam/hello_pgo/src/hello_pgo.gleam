import gleam/dynamic
import gleam/io
import gleam/option.{Some}
import gleam/pgo.{type Connection}

fn cfg() {
  pgo.Config(
    ..pgo.default_config(),
    host: "127.23.0.1",
    password: Some("postgres"),
    database: "mpg_db",
  )
}

fn get_list_exn(pool: Connection) {
  let sql = "SELECT x AS n FROM generate_series(1,3)x"
  let decoder = dynamic.element(0, dynamic.int)
  let assert Ok(response) = pgo.execute(sql, pool, [], decoder)
  response.rows
}

fn get_top3_customers_exn(pool: Connection) {
  let sql =
    "
    SELECT id, name, alternative_name
    FROM customers
    LIMIT $1
    "
  let decoder =
    dynamic.tuple3(
      dynamic.int,
      dynamic.string,
      dynamic.optional(dynamic.string),
    )
  let assert Ok(response) = pgo.execute(sql, pool, [pgo.int(3)], decoder)
  response.rows
}

pub fn main() {
  let pool = pgo.connect(cfg())
  io.debug(#("Numbers", get_list_exn(pool)))
  io.debug(#("Top 3 customers", get_top3_customers_exn(pool)))
  io.println("Done!")
}

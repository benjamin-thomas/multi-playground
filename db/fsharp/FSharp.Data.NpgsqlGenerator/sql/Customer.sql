/*
{
  "name": "GetCustomerByName",
  "isPrepared": false,
  "singleRow": false
}
*/
SELECT * FROM customers WHERE name = @name;

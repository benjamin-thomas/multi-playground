require "socket"
require "logger"

=begin
DEBUG=1 bundle exec ruby ./server.rb --directory /tmp/

rg --files | entr -cr bash -c 'DEBUG=1 bundle exec ruby ./server.rb --directory /tmp/'
=end

=begin
TODO: Investigate the line separator $/
=end

def handle_client(client)
  request_line = client.gets
  $logger.info("request_line: #{request_line}")

  verb, path, _ = request_line.split
  $logger.info("verb: #{verb}, path: #{path}")

  headers = {}
  loop do
    line = client.gets
    break line if line == "\r\n"

    key, value = line.split(":", 2).map(&:strip)
    headers[key] = value
  end

  # headers =
  #   client
  #     .each_line
  #     .take_while { |line| line != "\r\n" }
  #     .reduce({}) do |acc, line|
  #       k, v = line.split(":", 2).map(&:strip)
  #       acc[k] = v
  #       acc
  #     end

  $logger.info("headers: #{headers}")

  case [verb, path.split("/").drop(1)]
  in ["GET", []]
    client.print("HTTP/1.1 200 OK\r\n")
  in ["GET", ["echo", msg]]
    client.print("HTTP/1.1 200 OK\r\n")
    client.print("Content-Type: text/plain\r\n")
    client.print("Content-Length: #{msg.bytesize}\r\n")
    client.print("\r\n")
    client.print(msg)
  in ["GET", ["user-agent"]]
    user_agent = headers["User-Agent"]
    client.print("HTTP/1.1 200 OK\r\n")
    client.print("Content-Type: text/plain\r\n")
    client.print("Content-Length: #{user_agent.bytesize}\r\n")
    client.print("\r\n")
    client.print(user_agent)
  in ["GET", ["files", filename]]
    filepath = File.join($files_dir, filename)

    if !File.exist?(filepath)
      client.print("HTTP/1.1 404 Not Found\r\n")
    else
      client.print("HTTP/1.1 200 OK\r\n")
      client.print("Content-Type: application/octet-stream\r\n")
      client.print("Content-Length: #{File.size(filepath)}\r\n")
      client.print("\r\n")
      client.print(File.read(filepath))
    end
  in ["POST", ["files", filename]]
    len = headers["Content-Length"].to_i
    if len == 0 || headers["Content-Type"] != "application/octet-stream"
      client.print("HTTP/1.1 400 Bad Request\r\n")
    else
      content = client.read(len)
      filepath = File.join($files_dir, filename)
      File.write(filepath, content)

      client.print("HTTP/1.1 201 Created\r\n")
      client.print("Content-Type: text/plain\r\n")
      client.print("Content-Length: #{content.bytesize}\r\n")
      client.print("\r\n")
      client.print(content)
    end
  else
    client.print("HTTP/1.1 404 Not Found\r\n")
  end

  client.print("\r\n")
  client.close
end

def start
  $logger.info("Booting up...")
  server = TCPServer.new("localhost", 4221)

  loop do
    # handle_client(server.accept)
    Thread.start(server.accept) { handle_client(_1) }
  end
end

$logger = Logger.new(STDOUT)

if ENV["DEBUG"] == "1"
  $logger.info("Loading debug extensions...")
  require "pry"
  require "pry-doc"
end

def arg_or(args, key, default)
  case args
  in []
    default
  in [^key, val, *]
    val
  else
    arg_or(args.drop(1), key, default)
  end
end

$files_dir = arg_or(ARGV, "--directory", "/dev/null")
$logger.info("Files directory: #{$files_dir}")

start()

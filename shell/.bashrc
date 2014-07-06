function server() {
    local port="${1:-8080}"
    chromium "http://localhost:${port}/"
    twistd -n web --path .  --port "$port"
}

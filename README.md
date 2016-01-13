# Scalarlog

A Yesod web app that receives time-varying scalar data and plots it.

## Starting the server

    $ SCALARLOG_DB_FILE=database.sqlite \
      SCALARLOG_API_KEY=my-api-key \
      stack exec scalarlog-exe

Verify that the server is running at [`http://localhost:3000`](http://localhost:3000).

## Defining quantities

As an example, we'll be logging CPU usage in percent:

    $ sqlite3 database.sqlite
    sqlite> INSERT INTO tag (name, unit) VALUES ('cpu', '%');

No UI for this yet!

## Storing data

Using any HTTP client, POST form data like the following to `http://localhost:3000/cpu`:

    apiKey=my-api-key
    time=<timestamp>
    value=<float>

### Bash+curl example (OS X)

    #!/bin/bash    
    url=http://localhost:3000/cpu
    api_key=my-api-key
    interval=5
    
    while read line; do
      if [[ $line =~ ^CPU\ usage:\ ([^%]+)%\ user,\ ([^%]+)%\ sys, ]]; then
        user_cpu=${BASH_REMATCH[1]}
        sys_cpu=${BASH_REMATCH[2]}
    
        cpu=$(echo "$user_cpu + $sys_cpu" | bc)
        timestamp=$(date +'%Y-%m-%dT%H:%M:%S%z')
    
        curl \
          --data-urlencode "apiKey=$api_key" \
          --data-urlencode "time=$timestamp" \
          --data-urlencode "value=$cpu" \
          "$url"
      fi
    done < <(top -F -R -l 0 -n 0 -s "$interval")

## Viewing the plot

Open [`http://localhost:3000/cpu`](http://localhost:3000/cpu) in your browser:

![Example plot](http://i.imgur.com/r7Cy9W2.png)

## TODO

TLS, plot scalability, admin tools…

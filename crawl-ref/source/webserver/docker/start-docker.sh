# Starts the container in interactive mode
docker run -p 8080:8080 -it -v $(pwd)/../../..:/crawl crawl:webserver /bin/bash
# Builds the docker container. Only needs to be run once unless system
# dependencies (or any utility scripts) change. The container is tagged
# locally and stored in your docker cache.
docker build -t crawl:webserver .
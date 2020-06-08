#!/bin/bash

docker build . -t "$1"
docker push "$1"
aws ecs update-service --cluster callumscode-2-cluster --service callumscode-2-web-service --force-new-deployment

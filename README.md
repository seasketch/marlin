# Running marlin in AWS Lambda

Lambda-based worker for running the R package `marlin` and integrating with `geoprocessing`. 

This system relies on Lambda's support for containerized functions using a `Dockerfile` so that R can be included in the runtime environment. 
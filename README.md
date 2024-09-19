# Running marlin in AWS Lambda

Lambda-based worker for running the R package [marlin](https://github.com/DanOvando/marlin) and integrating with [geoprocessing](https://github.com/seasketch/geoprocessing). 

This system relies on Lambda's support for containerized functions using a `Dockerfile` so that R can be included in the runtime environment, and [lambdr](https://github.com/mdneuzerling/lambdr) which coordinates I/O between R and AWS. 


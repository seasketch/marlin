FROM public.ecr.aws/lambda/provided:latest-x86_64

ENV R_VERSION=4.3.2

RUN dnf -y install wget git tar

RUN wget https://cdn.rstudio.com/r/rhel-9/pkgs/R-${R_VERSION}-1-1.x86_64.rpm

# system requirements for R
RUN dnf install -y `rpm -qR R-${R_VERSION}-1-1.x86_64.rpm | tr '\n' ' ' `

RUN rpm -i R-${R_VERSION}-1-1.x86_64.rpm \
    && rm R-${R_VERSION}-1-1.x86_64.rpm

ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"

# Install build tools
RUN dnf install -y openssl-devel libcurl-devel libxml2-devel zlib-devel bzip2-devel xorg-x11-server-devel gcc gcc-c++ gfortran make lapack-devel blas-devel

# Install additional R packages
RUN Rscript -e "install.packages(c('httr', 'jsonlite', 'logger', 'remotes'), repos = 'https://packagemanager.rstudio.com/all/__linux__/centos7/latest')"
RUN Rscript -e "remotes::install_github('mdneuzerling/lambdr')"

# Install marlin with try-catch for error handling
RUN Rscript -e "tryCatch({ remotes::install_github('DanOvando/marlin'); if (!('marlin' %in% installed.packages())) { stop('marlin installation failed') } else { cat('marlin successfully installed\\n') } }, error = function(e) { cat('Error during marlin installation:', conditionMessage(e), '\\n'); quit(status = 1) })"

# Check if marlin is installed successfully
RUN Rscript -e "if (!('marlin' %in% installed.packages())) { stop('marlin installation failed') } else { print('marlin successfully installed') }"

RUN mkdir /lambda
COPY runtime.R /lambda
RUN chmod 755 -R /lambda

RUN printf '#!/bin/sh\ncd /lambda\nRscript runtime.R' > /var/runtime/bootstrap \
    && chmod +x /var/runtime/bootstrap

CMD ["run_marlin"]
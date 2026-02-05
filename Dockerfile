FROM ubuntu:22.04

# Install system dependencies
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y \
        curl gzip nano less wget gpg lsb-release \
        python3-pip python3.10-venv \
        make maven \
        fonts-liberation fonts-dejavu fonts-liberation2 fontconfig && \
    fc-cache -fv && \
    apt-get clean

    SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
RUN chmod +x cs
RUN mv cs /usr/local/bin/cs
RUN cs setup --env --jvm 25 sbt --yes >> ~/.bashrc

# Copy the project files
WORKDIR /workspace
COPY . /workspace

# Install Python plotting dependencies
RUN make plot-install
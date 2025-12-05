FROM ubuntu:22.04
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y curl gzip nano less wget gpg lsb-release python3-pip python3.10-venv make \
    fonts-liberation fonts-dejavu fonts-liberation2 fontconfig && \
    curl -fL 'https://proget.makedeb.org/debian-feeds/prebuilt-mpr.pub' | gpg --dearmor | tee /usr/share/keyrings/prebuilt-mpr-archive-keyring.gpg 1> /dev/null && \
    echo "deb [arch=all,$(dpkg --print-architecture) signed-by=/usr/share/keyrings/prebuilt-mpr-archive-keyring.gpg] https://proget.makedeb.org prebuilt-mpr $(lsb_release -cs)" | tee /etc/apt/sources.list.d/prebuilt-mpr.list && \
    apt-get update -y && \
    apt-get install just -y && \
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
FROM ubuntu:latest

# Install needed packages
RUN apt-get update && \
	apt-get install -y locales wget unzip && \
  locale-gen en_US.UTF-8

# Set up UTF-8 locale
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Set the working directory to /build
WORKDIR /build

# Copy the project configuration
COPY package.yaml stack.yaml stack.yaml.lock ./
RUN mkdir -p src app

# Setup stack and install dependencies
RUN wget -qO- https://get.haskellstack.org/ | sh && \
  stack build --dependencies-only

# Build the application
COPY . .
RUN stack build --copy-bins

FROM ubuntu:latest
COPY --from=0 /root/.local/bin/callumscode2 /callumscode2
COPY static/ /static/
ENV STATIC_DIR /static
ENV PORT 80
ENV IP_FROM_HEADER true
EXPOSE 80
ENTRYPOINT ["/callumscode2"] 


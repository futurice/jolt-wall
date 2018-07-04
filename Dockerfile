FROM node:10

EXPOSE 8000

# install node http server
RUN npm install -g http-server

# Create directory for app
RUN mkdir -p /jolt-wall

# Change to the jolt-wall directory
WORKDIR /jolt-wall

# Copy the built elm app to docker.
# This is safe, as elm runs on browser so it does not contain platform-specific binaries
COPY build /jolt-wall/build/
# Change file permissions
RUN chown -R node /jolt-wall

# Drop root privileges. everything after this will be done as user node
USER node

# serve the static content from build directory
CMD ["http-server", "/jolt-wall/build", "-p", "8000"]

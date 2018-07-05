FROM node:10

EXPOSE 8000

# Change to the jolt-wall directory and change it's permissions
# WORKDIR creates the directory behind the curtains
WORKDIR /jolt-wall
RUN chown node:node /jolt-wall

# install node http server
RUN npm install -g http-server

# Drop root privileges. everything after this will be done as user node
USER node

# Copy the built elm app to docker.
# This is safe, as elm runs on browser so it does not contain platform-specific binaries
COPY --chown=node:node build /jolt-wall/build/

# serve the static content from build directory
CMD ["http-server", "/jolt-wall/build", "-p", "8000"]

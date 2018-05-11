FROM node:8.11.1
RUN npm install http-server -g

COPY ./build /
EXPOSE 8000
CMD ["http-server", "-p", "8000"]

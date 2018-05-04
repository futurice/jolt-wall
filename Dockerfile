FROM ubuntu:16.04
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y \
    python

COPY ./build /
EXPOSE 8000
CMD ["python", "-m", "SimpleHTTPServer", "8000"]

FROM alpine:3
RUN apk add cmake gcc g++ libgcc build-base make --no-cache
WORKDIR /app/
COPY . /app/
WORKDIR /app/build
RUN cmake ..
RUN make -j4

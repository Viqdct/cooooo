FROM gcc:10
WORKDIR /app/
COPY ./* ./
RUN gcc main.cpp -o main
RUN chmod +x main

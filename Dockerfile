FROM g++:10
WORKDIR /app/
COPY ./* ./
RUN g++ main.cpp -o main
RUN chmod +x main

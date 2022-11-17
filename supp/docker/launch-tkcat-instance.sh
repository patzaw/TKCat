docker build -t tkcat .

docker run -d \
	--name tkcat_container \
	--ulimit nofile=262144:262144 \
	--publish=9000:9000 \
	--publish=8123:8123 \
	--restart=always tkcat
	
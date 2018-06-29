fetch: images/dog.png images/cat.png

clean:
	rm -r images

images/dog.png:
	mkdir -p images
	wget "https://api.thedogapi.com/v1/images/search?format=src&mime_types=image/png" -O images/dog.png

images/cat.png:
	mkdir -p images
	wget "http://thecatapi.com/api/images/get?format=src&type=png" -O images/cat.png

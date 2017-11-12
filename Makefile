build:
	stack exec site build

rebuild:
	stack exec site rebuild

monitor:
	stack exec site watch

clean:
	stack exec site clean

deploy:
	cd _site && aws --profile=blog s3 sync --delete --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --exclude="images/*" --exclude="css/*" --exclude="files/*" . s3://dannysu.com
	cd _site && aws --profile=blog s3 sync --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --cache-control="max-age=691200" --exclude="*" --include="images/*" --include="css/*" --include="files/*" . s3://dannysu.com

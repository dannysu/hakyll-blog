require 'fileutils'

task :build do
  sh "stack build"
  sh "stack exec site build"
end

task :monitor do
  sh "stack exec site watch"
end

task :clean do
  sh "stack exec site clean"
end

task :deploy do
  sh "cd _site && aws --profile=blog s3 sync --delete --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --exclude=\"images/*\" --exclude=\"css/*\" --exclude=\"files/*\" . s3://dannysu.com"
  sh "cd _site && aws --profile=blog s3 sync --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --cache-control=\"max-age=691200\" --exclude=\"*\" --include=\"images/*\" --include=\"css/*\" --include=\"files/*\" . s3://dannysu.com"
end

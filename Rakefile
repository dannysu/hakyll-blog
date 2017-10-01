require 'fileutils'

def ensure_index(directory, file_404)
    index_path = File.join(directory, "index.html")
    if not File.exists?(index_path)
        FileUtils.copy(file_404, index_path)
    end

    Dir.foreach(directory) do |item|
        new_path = File.join(directory, item)
        next if item == '.' or item == '..' or not File.directory?(new_path)
        ensure_index(new_path, file_404)
    end
end

task :build do
  sh "stack build"
  sh "stack exec site rebuild"

  # Make sure every directory without index.html has one
  ensure_index(File.expand_path('./_site'), File.expand_path('./_site/404.html'))
end

task :monitor do
  sh "stack exec site watch"
end

task :clean do
  sh "stack exec site clean"
end

task :deploy do
  sh "cd _site && aws --profile=blog s3 sync --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --exclude=\"images/*\" --exclude=\"css/*\" --exclude=\"files/*\" . s3://dannysu.com"
  sh "cd _site && aws --profile=blog s3 sync --storage-class=REDUCED_REDUNDANCY --acl=public-read --region=us-west-2 --cache-control=\"max-age=691200\" --exclude=\"*\" --include=\"images/*\" --include=\"css/*\" --include=\"files/*\" . s3://dannysu.com"
end

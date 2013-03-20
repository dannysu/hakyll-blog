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
  # Build Hakyll blog code
  sh "ghc --make site.hs"

  sh "./site rebuild"

  # Make sure every directory without index.html has one
  ensure_index(File.expand_path('./_site'), File.expand_path('./_site/404.html'))

  # Update dannysu.github.com submodule
  sh "rm -rf dannysu.github.com/*"
  sh "cp -r _site/* dannysu.github.com"
  sh "cd dannysu.github.com && git checkout CNAME && git checkout .nojekyll"
end

task :monitor do
  sh "./site preview"
end

task :clean do
  sh "./site clean"
end

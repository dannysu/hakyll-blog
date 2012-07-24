task :build do
  sh "ghc site.hs"
  sh "./site rebuild"
  sh "cp _site/recent_template.html recent.markdown"
  sh "./site rebuild"
  sh "rm _site/recent_template.html"
  sh "rm -rf dannysu.github.com/*"
  sh "cp -r _site/* dannysu.github.com"
  sh "cd dannysu.github.com && git checkout CNAME && git checkout .nojekyll"
end

task :monitor do
  sh "./site preview"
end

task :clean do
  sh "git checkout recent.markdown"
  sh "./site clean"
end

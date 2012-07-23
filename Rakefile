task :build do
  sh "ghc site.hs"
  sh "./site rebuild"
  sh "cp _site/recent_template.html recent.markdown"
  sh "./site rebuild"
  sh "rm _site/recent_template.html"
end

task :monitor do
  sh "./site preview"
end

task :clean do
  sh "git co recent.markdown"
  sh "./site clean"
end

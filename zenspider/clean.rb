#!/usr/bin/ruby -w

old_result = nil

ARGF.each_line do |line|
  line.chomp!
  line.sub!(%r%/Users/ryan/Work/git/zenspider/schemers/little-schemer%, ".")
  line.sub!(%r%wtf\d\d%, "wtf##")

  case line
  when /^ *>>/ then
    old_result = nil # reset
  when /^ *<</ then
    # do nothing
  when /^ *result: (.*)/ then
    result = $1
    line.sub!(/result:.*/, "<same>") if result == old_result
    # next if result == old_result
    old_result = result
  end

  line.sub!(/^/, ";; ")
  puts line
end

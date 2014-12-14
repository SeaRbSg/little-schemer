module Parser
  REGEXP_PARSE_NEXP = /(\([[:alnum:]]+\s*[x\-\+\^]\s*[[:alnum:]]+\))/

  def self.arrafy(number_exp)
    clean_exp = number_exp.strip.chomp.gsub(/\s+/, ' ')
    reconstitute_as_arrays(*extract_expressions(clean_exp))
  end

  def self.extract_expressions(number_exp)
    dictio = {}
    loop do
      return [[number_exp], dictio] unless number_exp.match(REGEXP_PARSE_NEXP)
      keycode = $1.hash.abs
      number_exp.sub!(REGEXP_PARSE_NEXP, "#{keycode}")
      dictio["#{keycode}"] = $1.sub(/^\(/, '').sub(/\)$/, '').split(' ')
    end
  end

  def self.reconstitute_as_arrays(arry, dictio)
    return reconstitute_as_arrays(arry) if arry.kind_of? String
    arry.map { |n| dictio[n] ? reconstitute_as_arrays(dictio[n], dictio) : n }
  end
end

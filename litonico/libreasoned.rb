Variable = Struct.new :name

def is_variable? var
  var.is_a? Variable
end

empty_substitution = Hash.new

def walk var, sub_list
  if is_variable? var
    a = sub_list[var]
    unless a.nil?
      a
    else
      var
    end
  else
    var
  end
end

def reify substitution
end

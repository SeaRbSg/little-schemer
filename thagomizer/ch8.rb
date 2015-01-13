class Array
  def rest
    self[1..-1]
  end

  def cons x
    self.unshift(x)
  end
end

def multirember_and_co a, lat, collector
  case
  when lat.empty?
    collector.call([], [])
  when lat[0] == a
    multirember_and_co(a,
                   lat.rest,
                   Proc.new { |newlat, seen|
                     collector.call(newlat, seen.cons(lat.first))})
  else
    multirember_and_co(a,
                       lat.rest,
                       Proc.new { |newlat, seen|
                         collector.call(newlat.cons(lat.first), seen) })
  end
end

module Complement
  DNA_2_RNA = { 'G' => 'C', 'C' => 'G', 'T' => 'A', 'A' => 'U' }

  RNA = Struct.new(:strand) do
    def to_dna
      DNA.new(strand.chars.map { |n| DNA_2_RNA.invert[n] }.join)
    end
  end

  DNA = Struct.new(:strand) do
    def to_rna
      RNA.new(strand.chars.map { |n| DNA_2_RNA[n] }.join)
    end
  end

  def self.of_dna(strand)
    DNA.new(strand).to_rna.strand
  end

  def self.of_rna(strand)
    RNA.new(strand).to_dna.strand
  end
end

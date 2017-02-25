require 'sinatra'
require 'json'
require './main'

get '/' do
  erb :index
end

post '/possible-matches' do
  begin
    mass_type = MassTypes.parse params[:mass_type]
    residues = AminoAcids.new(mass_type).residues
    p = Peptide.new params[:peptide_sequence], residues, params[:wildcards] || {}
    possible_sequences = p.possible_sequences params[:weight].to_f
    { 'weight' => p.weight, 'possible_sequences' => possible_sequences }.to_json
  rescue BadSequenceError => e
    { 'possible_sequences' => e.message }.to_json
  rescue => e
    { 'possible_sequences' => e.message }.to_json
  end
end

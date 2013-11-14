require 'sinatra'
require 'json'
require './main'

get '/' do
  erb :index
end

post '/possible-matches' do
  p = Peptide.new params[:peptide_sequence]
  possible_sequences = p.possible_sequences params[:weight].to_f
  { 'possible_sequences' => possible_sequences }.to_json
end

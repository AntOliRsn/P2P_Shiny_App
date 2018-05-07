market_description <- function(){
  div(class="info",
    tags$p("Today, electricity consumers have to buy their energy supply from a retailer. The deployment of
renewable energy generation capacities down to the house
and building levels, possibly with storage, makes that consumers becoming prosumers.
“Prosumer” is the common term to refer to those who are both
able to produce and consume electricity (some talk about “prosumage” if
storage is added). As of now prosumers directly consume their electricity
generation while pushing their electricity surplus to the electric grid under
fairly strict regulation. Surplus power generation is often not bought from
the grid at an advantageous rate. When the share of prosumers in the system is representative (also more
generally of distributed energy resources), why wouldn’t we think that it
is possible to directly exchange electricity from producers to consumers,
among neighbours or directly form a wind farm to a consumer? This seemingly simple 
question raises the more fundamental issue of re-thinking the
electricity market in a more flexible manner and adapting to the new energy
production and consumption landscape. Going to the most flexible structure
that one may envisage, the market would then have a peer-to-peer (P2P)
exchange mechanism as a backbone, where any producer/consumer could
negotiate with any other agent of the system for buying and selling electric
energy. This would eventually represent a complete switch from the existing top-down 
structure to fully decentralized market structure, also truly
consumer-centric."),
    
    tags$img(src="Fig_transaction_decentralized_setup.jpg",  width = "500px", height = "272px", class="center"),
    
    tags$p("Peer-to-peer concepts were reborn in the 90s as a decentralized and
democratic approach to sharing audio files, while the original concept of
bilateral trading and direct negotiations among potential buyers and sellers. 
Think of the way we buy second-hand items today. It is highly likely
that such a peer-to-peer approach will enter the electricity sector, thanks
to recent innovations in home automation, information and communication
technology, etc. The main benefit of this promising P2P electricity market
is the empowerment of consumers, allowing them to express their prefer-
ences: instead of seeing electricity as an homogeneous good (an electron is
an electron!), it can then be differentiated and consumers may prefer to buy
more solar power locally, wind power from specific offshore wind farms, etc.
Many novel and disruptive business models may be proposed by having this
peer-to-peer market mechanism in the background: charge your electric car
with solar power only, design and operate energy communities, local energy
consumption in urban environment, etc. This organic approach to future
electricity markets is expected to be a game-changer in improving consumer
involvement to the current energy transition."),
    tags$p("We developped this R-Shiny app for a broader audience to understand
and appraise the potential and implications of P2P electricity markets. Each
consumer (here, households) can define a set of preferences to reflect his/her
willingness to exchange more with local and/or green energy producers. A
negotiation mechanism that we designed and validated at the Technical University 
of Denmark (DTU) permits to reach a set of exchanges and related
contracts (i.e., amount of energy and related price) between producers and
consumers given the set of preferences they defined originally. The user can
visualize the exchange of energy between different producers and consumers,
as well as the characteristics of the trade and overall statistics on the exchanges.
For instance, the amount of local and green energy that consumers
are provided with can be seen and analysed."),
    tags$p("Any feedback and suggestion on how to improve this R-Shiny app is
welcome. Fell free to contact us at DTU if you would like to know more
about this topic and our activities in that field.")
  )
}

energy_geeks <- function(){
  div(class="info",
    tags$p("Three technical factors are behind the recent rise of this new form of electricity market. 
           First of all, the rapid and substantial deployment of renewable energy generation capacities, 
e.g. wind farms and mainly solar panels, had a substantial impact on the energy generation landscape. 
  In parallel, other distributed energy resources like storage units, heat pumps and electric vehicles bring new 
types of energy consumption and flexibility on the consumption side.
           Finally, the digitization of the energy sector thanks to the smart meters, new actuators and control, and 
recent discussion about the blockchain, bring some new perspective on how we are all connected.
           All of those aspects have made us rethink how we produce and consume electricity, also how it may flow 
through electric power networks.
           However, it has not made us rethink how we trade and exchange electricity energy, until fairly recently. 
Some may also say that principles from sharing and collaborative economy also support the proposals for a new view of electricity markets.
           Why not just going peer-peer?"),
    tags$p("P2P electricity markets can rely on varied designs. 
  Some of the most relevant and likely approaches for actual deployment and operation range from
  fully peer-to-peer decentralized approaches to community-based approaches, with a potential combination of both. 
A full P2P setup assumes that all agents are directly interacting with each other instead of going through any intermediary agent.
"),
    
    tags$img(src="Fig_Full_P2P_Setup.jpg",  width = "500px", height = "272px", class="center"),
    
    tags$p("Two agents, consumer and producer, are to agree on an amount of energy to exchange at a given price,
           this price being potentially different from that used in other transactions. Also, there is no restriction
           in the way a given agent may sell to, or buy from which, a unique or a large number of other agents. 
           In addition, it allows for negotiation and settlement based on additional preferences, and not on 
           monetary considerations only. For example, consider that one prosumer prefers to buy wind energy; 
           another one favours selling to local consumers, while a third one requires only consumption from
           a given solar PV plant. This cannot be done under current electricity market structures though it 
           would be natural if switching to P2P market mechanisms."),
    tags$p("Another setup is the community-based approach that relies on a community of agents, geographically 
           close and/or with common interests. This setup allows to exchange energy within the community under a
           given social contract, to make an agreement on how to interact with the grid and potential suppliers
           of the community (if needed), possibly also to manage common investments in energy assets (shared PV, 
           storage, heat pumps, etc.). This community is seen as a single node that interacts with rest of the 
           electricity market. P2P and community-based approaches are complementary and can co-exist in the future."),
    
    tags$img(src="Fig_Community_Setup.jpg",  width = "500px", height = "272px", class="center"),
    
    tags$p("Over the last few years, a number of iconic demonstration projects have focused on pushing novel concepts 
related to consumer-centric and P2P electricity markets. The legal and regulatory framework comprises on of the main barriers 
for P2P markets, since in most countries it is illegal to directly
buy/sell energy produced from your neighbours, a wind farm, etc. since it is the role of retailers to channel 
           the trading activities towards final consumers. For this reason virtual and crypto-currencies, like NRGcoin, 
           solarCoin, etc, have been used in these demonstrators. Recently, some countries have closely looked at these 
           legal barriers, as for the French example where a law passed early 2017 to support collaborative self-consumption 
           of renewable power generation as a community. Since P2P electricity markets are fundamentally decentralized, many 
           see blockchain as natural platform and enabler for registering and settling energy transactions. It is not a requirement 
           to use blockchain though, as other platform types may be considered to support P2P markets e.g. in the cloud.")
  )
}


app_guidelines <- function(){
  div(class="info",
    h3("Simulation"),
    p( tags$b("Solver:"), "RCI (iterative process) developped by the ELMA Group (DTU)."),
    p( tags$b("General information:"), "The solver will solve the optimisation problem given the setup characterisics and the set of preferences defined for each consumer."),
    p( tags$b("Remark:"), "If the local preferences are set to zero for most of the consumers, the simulation takes some time (~10s)."),
    
    h3("Preferences"),
    p( tags$b("General information:"), "It is possible to modify the preferences of each consumer (defined by a value between 0 and 1)."),
    p( tags$b("Preference types:")),
    tags$ul(
      tags$li("Distance: the more important the preference is, the more the agent wants to consume local power."),
      tags$li("Emission: the more important the preference is, the more the agent wants to consume green energy.")
    ),
    p( tags$b("Remark:"), "The top red slidder changes the selected preference of all the consumers of the selected group.")
  )
}




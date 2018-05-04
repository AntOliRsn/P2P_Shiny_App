market_description <- function(){
  h3("P2P markets description")
  div(
    tags$p("Today, electricity consumers have to buy their energy supply from a retailer.
The deployment of renewable energy generation capacities down to the house
and building levels, possibly with storage, makes that consumers becoming
prosumers. “Prosumer” is the common term to refer to those who are both
able to produce and consume electricity (some talk about “prosumage” if
storage is added). As of now prosumers directly consume their electricity
generation while pushing their electricity surplus to the electric grid under
fairly strict regulation. Surplus power generation is often not bought from
the grid at an advantageous rate."),
    tags$p("When the share of prosumers in the system is representative (also more
generally of distributed energy resources), why wouldn’t we think that it
is possible to directly exchange electricity from producers to consumers,
among neighbours or directly form a wind farm to a consumer? This seem-
ingly simple question raises the more fundamental issue of re-thinking the
electricity market in a more flexible manner and adapting to the new energy
production and consumption landscape. Going to the most flexible structure
that one may envisage, the market would then have a peer-to-peer (P2P)
exchange mechanism as a backbone, where any producer/consumer could
negotiate with any other agent of the system for buying and selling electric
energy. This would eventually represent a complete switch from the exist-
ing top-down structure to fully decentralized market structure, also truly
consumer-centric."),
    tags$p("Peer-to-peer concepts were reborn in the 90s as a decentralized and
democratic approach to sharing audio files, while the original concept of
bilateral trading and direct negotiations among potential buyers and sell-
ers. Think of the way we buy second-hand items today. It is highly likely
that such a peer-to-peer approach will enter the electricity sector, thanks
to recent innovations in home automation, information and communication
technology, etc. The main benefit of this promising P2P electricity market
is the empowerment of consumers, allowing them to express their prefer-
ences: instead of seeing electricity as an homogeneous good (an electron is
1an electron!), it can then be differentiated and consumers may prefer to buy
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
negotiation mechanism that we designed and validated at the Technical Uni-
versity of Denmark (DTU) permits to reach a set of exchanges and related
contracts (i.e., amount of energy and related price) between producers and
consumers given the set of preferences they defined originally. The user can
visualize the exchange of energy between different producers and consumers,
as well as the characteristics of the trade and overall statistics on the ex-
changes. For instance, the amount of local and green energy that consumers
are provided with can be seen and analysed."),
    tags$p("Any feedback and suggestion on how to improve this R-Shiny app is
welcome. Fell free to contact us at DTU if you would like to know more
about this topic and our activities in that field.")
  )
}
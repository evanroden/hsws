'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

interface Layer {
  id: string
  name: string
  color: string
  description: string
  features: { label: string; detail: string }[]
}

const layers: Layer[] = [
  {
    id: 'disaster',
    name: 'Disaster Planning',
    color: '#B87333',
    description:
      'Comprehensive flood mitigation and hurricane resilience strategy for New Orleans East, one of the areas most devastated by Hurricane Katrina. Elevated building standards, improved drainage infrastructure, and community emergency preparedness programs.',
    features: [
      { label: 'Elevated Construction', detail: 'All new builds 3+ ft above base flood elevation' },
      { label: 'Green Stormwater', detail: 'Bioswales, rain gardens, and permeable surfaces' },
      { label: 'Emergency Shelters', detail: 'Distributed resilience hubs with backup power' },
      { label: 'Evacuation Routes', detail: 'Improved signage and multi-modal evacuation corridors' },
    ],
  },
  {
    id: 'solar',
    name: 'Solar Energy',
    color: '#D4A54A',
    description:
      'Distributed solar generation across residential rooftops, commercial buildings, and community solar farms. Designed to lower energy costs for residents while building grid resilience against future storms.',
    features: [
      { label: 'Community Solar', detail: '5 MW community solar farm on vacant parcels' },
      { label: 'Rooftop Program', detail: 'Subsidized residential installations for 500+ homes' },
      { label: 'Battery Storage', detail: 'Neighborhood-scale battery systems for outage resilience' },
      { label: 'Net Metering', detail: 'Revenue generation for participating households' },
    ],
  },
  {
    id: 'biogas',
    name: 'Biogas',
    color: '#6B8F71',
    description:
      'Waste-to-energy biogas generation from organic waste streams. Converts landfill-bound material into renewable energy while reducing methane emissions and creating local jobs in waste processing.',
    features: [
      { label: 'Anaerobic Digester', detail: 'Processing 50 tons/day of organic waste' },
      { label: 'CNG Fleet', detail: 'Compressed natural gas fueling for municipal vehicles' },
      { label: 'Compost Program', detail: 'Digestate converted to agricultural compost' },
      { label: 'Emissions Reduction', detail: 'Estimated 12,000 tons CO2e avoided annually' },
    ],
  },
  {
    id: 'transit',
    name: 'BRT Transit',
    color: '#4A90D9',
    description:
      'Bus Rapid Transit corridor connecting New Orleans East to downtown and key employment centers. Dedicated lanes, signal priority, and level boarding to provide reliable, fast public transit.',
    features: [
      { label: 'Dedicated Lanes', detail: '8.5-mile BRT corridor on Chef Menteur Highway' },
      { label: 'Station Design', detail: '12 stations with shelters, real-time info, and lighting' },
      { label: 'Signal Priority', detail: 'Transit signal priority at all intersections' },
      { label: 'Frequency', detail: '10-minute headways during peak, 15-minute off-peak' },
    ],
  },
  {
    id: 'bike',
    name: 'Bike Infrastructure',
    color: '#7BC8A4',
    description:
      'Protected bike lanes, multi-use paths, and bike share stations to provide safe, affordable transportation alternatives and connect neighborhoods to transit stations and commercial centers.',
    features: [
      { label: 'Protected Lanes', detail: '15 miles of protected bike lanes on arterials' },
      { label: 'Multi-Use Path', detail: 'Bayou Sauvage greenway trail system' },
      { label: 'Bike Share', detail: '20 Blue Bikes stations throughout the district' },
      { label: 'Secure Parking', detail: 'Covered bike parking at all BRT stations' },
    ],
  },
  {
    id: 'community',
    name: 'Community Development',
    color: '#C97B63',
    description:
      'Neighborhood-scale investments in public spaces, community centers, small business incubators, and cultural programming. Designed to rebuild social infrastructure alongside physical infrastructure.',
    features: [
      { label: 'Community Centers', detail: '3 new multi-purpose neighborhood centers' },
      { label: 'Public Spaces', detail: 'Pocket parks, plazas, and community gardens' },
      { label: 'Small Business', detail: 'Micro-enterprise incubator and co-working space' },
      { label: 'Cultural Programs', detail: 'Arts programming and Vietnamese community heritage center' },
    ],
  },
  {
    id: 'housing',
    name: 'Green Housing',
    color: '#2D5A45',
    description:
      'Affordable, energy-efficient housing development using passive design principles, solar-ready construction, and resilient building materials. Targets 30% affordability set-aside in all new developments.',
    features: [
      { label: 'Passive Design', detail: 'Cross-ventilation, shading, and thermal mass' },
      { label: 'Affordable Units', detail: '30% of new units at or below 80% AMI' },
      { label: 'Energy Star', detail: 'All units built to Energy Star certification standards' },
      { label: 'Resilient Materials', detail: 'Flood-resistant construction and impact-rated windows' },
    ],
  },
  {
    id: 'jobs',
    name: 'Green Jobs',
    color: '#8A9BA8',
    description:
      'Workforce development pipeline for construction, solar installation, transit operations, and urban agriculture. Prioritizes hiring from New Orleans East residents and returning citizens.',
    features: [
      { label: 'Training Center', detail: 'Solar installation and green construction certification' },
      { label: 'Local Hire', detail: '50% local hiring requirement on all funded projects' },
      { label: 'Apprenticeships', detail: 'Paid apprenticeship pipeline with trade unions' },
      { label: 'Job Projections', detail: 'Estimated 1,200+ permanent jobs created' },
    ],
  },
]

export default function NolaEastPage() {
  const heroView = useInView(0.1)
  const mapView = useInView(0.05)
  const detailView = useInView(0.05)
  const [activeLayers, setActiveLayers] = useState<string[]>(['disaster', 'solar', 'transit'])
  const [selectedLayer, setSelectedLayer] = useState<string | null>(null)

  const toggleLayer = (id: string) => {
    setActiveLayers((prev) =>
      prev.includes(id) ? prev.filter((l) => l !== id) : [...prev, id]
    )
  }

  const activeLayerData = layers.filter((l) => activeLayers.includes(l.id))
  const selectedLayerData = selectedLayer ? layers.find((l) => l.id === selectedLayer) : null

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'NOLA East' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-forest/20 to-slate-950 overflow-hidden">
        <div className="absolute inset-0 overflow-hidden">
          <div
            className="absolute inset-0 opacity-[0.03]"
            style={{
              backgroundImage: 'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '40px 40px',
            }}
          />
        </div>
        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-forest-light mb-4 block">
              C40 Reinventing Cities Award
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              New Orleans East Revitalization
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Winner of the C40 Reinventing Cities Award from the Mayor of New Orleans. A comprehensive urban revitalization plan integrating disaster resilience, renewable energy, transit, and equitable community development.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">8</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Plan Layers</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">C40</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Award Winner</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">1,200+</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Jobs Projected</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">5 MW</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Community Solar</span>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Story Section */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={heroView.ref}>
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                The Context
              </span>
              <h2 className="font-serif text-heading text-white mb-8">
                Rebuilding a community that was left behind.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                New Orleans East is one of the largest geographic areas within the city, home to a diverse population that includes one of the largest Vietnamese-American communities in the South. It was also one of the areas most devastated by Hurricane Katrina in 2005 — and, nearly two decades later, large portions of the district remain underdeveloped with vacant lots, limited transit, and aging infrastructure.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                The C40 Reinventing Cities competition challenged teams to propose transformative, carbon-neutral development for underutilized urban sites. The New Orleans East proposal took a holistic approach: rather than treating sustainability as a single-issue problem, it integrated eight interconnected layers — from disaster planning and solar energy to green jobs and community development — into a unified vision.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                The proposal won the C40 Reinventing Cities Award from the Mayor of New Orleans, recognizing its ambition, feasibility, and commitment to equitable development. The plan was designed not just to build new infrastructure, but to create lasting economic opportunity for residents who have waited too long for reinvestment in their community.
              </motion.p>
            </div>
          </div>
        </div>
      </section>

      {/* Interactive Layer Map */}
      <section className="section-padding bg-slate-950" ref={mapView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={mapView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Interactive Plan
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Eight layers, one vision.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Toggle layers on and off to explore how each element of the revitalization plan integrates into the overall vision for New Orleans East.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
            {/* Layer toggles */}
            <motion.div
              initial={{ opacity: 0, x: -20 }}
              animate={mapView.isInView ? { opacity: 1, x: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
              className="space-y-2"
            >
              <span className="font-mono text-xs text-titanium/60 uppercase tracking-widest block mb-4">
                Toggle Layers
              </span>
              {layers.map((layer) => {
                const isActive = activeLayers.includes(layer.id)
                const isSelected = selectedLayer === layer.id

                return (
                  <div key={layer.id} className="flex items-center gap-2">
                    <button
                      onClick={() => toggleLayer(layer.id)}
                      className={`flex-1 flex items-center gap-3 px-4 py-3 rounded-lg text-left transition-all duration-300 ${
                        isActive
                          ? 'bg-white/10 border border-white/20'
                          : 'bg-white/[0.03] border border-transparent hover:bg-white/5'
                      } ${isSelected ? 'ring-1 ring-white/30' : ''}`}
                    >
                      <div
                        className={`w-3 h-3 rounded-sm transition-all duration-300 ${
                          isActive ? 'opacity-100' : 'opacity-30'
                        }`}
                        style={{ backgroundColor: layer.color }}
                      />
                      <span
                        className={`text-sm transition-colors ${
                          isActive ? 'text-white' : 'text-titanium/50'
                        }`}
                      >
                        {layer.name}
                      </span>
                    </button>
                    <button
                      onClick={() => setSelectedLayer(selectedLayer === layer.id ? null : layer.id)}
                      className={`px-2 py-3 rounded-lg text-xs font-mono transition-all ${
                        isSelected
                          ? 'bg-white/10 text-white'
                          : 'text-titanium/40 hover:text-titanium hover:bg-white/5'
                      }`}
                    >
                      info
                    </button>
                  </div>
                )
              })}
            </motion.div>

            {/* Map visualization */}
            <motion.div
              initial={{ opacity: 0 }}
              animate={mapView.isInView ? { opacity: 1 } : {}}
              transition={{ duration: 0.8, delay: 0.4 }}
              className="lg:col-span-2 glass rounded-xl p-6 md:p-8"
            >
              <div className="relative w-full" style={{ paddingBottom: '70%' }}>
                <svg viewBox="0 0 100 70" className="absolute inset-0 w-full h-full">
                  {/* New Orleans East district outline */}
                  <motion.path
                    d="M 15 15 L 85 15 Q 90 15 90 20 L 90 50 Q 88 55 80 55 L 65 58 Q 55 62 45 58 L 30 55 Q 20 52 15 48 L 10 35 Q 8 25 15 15 Z"
                    fill="rgba(27,58,45,0.08)"
                    stroke="rgba(138,155,168,0.2)"
                    strokeWidth="0.3"
                    initial={{ pathLength: 0, opacity: 0 }}
                    animate={mapView.isInView ? { pathLength: 1, opacity: 1 } : {}}
                    transition={{ duration: 2 }}
                  />

                  {/* Water / Lake Borgne */}
                  <motion.path
                    d="M 85 15 Q 95 20 95 35 Q 95 50 85 55"
                    fill="none"
                    stroke="rgba(74,144,217,0.15)"
                    strokeWidth="0.5"
                    initial={{ pathLength: 0 }}
                    animate={mapView.isInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 0.5 }}
                  />
                  <text x="92" y="35" textAnchor="middle" className="fill-titanium/15 font-mono" style={{ fontSize: '2px' }} transform="rotate(90 92 35)">
                    Lake Borgne
                  </text>

                  {/* Chef Menteur Highway */}
                  <motion.path
                    d="M 12 32 Q 35 28 55 30 Q 75 32 90 35"
                    fill="none"
                    stroke="rgba(138,155,168,0.15)"
                    strokeWidth="0.5"
                    strokeDasharray="1 0.5"
                    initial={{ pathLength: 0 }}
                    animate={mapView.isInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 0.8 }}
                  />
                  <text x="50" y="27" textAnchor="middle" className="fill-titanium/25 font-mono" style={{ fontSize: '1.8px' }}>
                    Chef Menteur Hwy
                  </text>

                  {/* Layer visualizations */}
                  {/* Disaster Planning - flood zones */}
                  {activeLayers.includes('disaster') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 0.4 }}
                      transition={{ duration: 0.5 }}
                    >
                      <rect x="20" y="45" width="55" height="10" rx="1" fill={layers[0].color} opacity="0.1" />
                      <text x="47" y="51" textAnchor="middle" className="font-mono" style={{ fontSize: '1.5px', fill: layers[0].color }}>
                        Flood Mitigation Zone
                      </text>
                    </motion.g>
                  )}

                  {/* Solar Energy - solar farm markers */}
                  {activeLayers.includes('solar') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      {[{ x: 30, y: 22 }, { x: 55, y: 20 }, { x: 70, y: 25 }].map((pos, i) => (
                        <motion.g key={`solar-${i}`}>
                          <motion.rect
                            x={pos.x - 2}
                            y={pos.y - 1.5}
                            width="4"
                            height="3"
                            rx="0.5"
                            fill={layers[1].color}
                            opacity="0.3"
                            animate={{ opacity: [0.2, 0.5, 0.2] }}
                            transition={{ repeat: Infinity, duration: 3, delay: i * 0.5 }}
                          />
                          <text x={pos.x} y={pos.y + 4} textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[1].color }}>
                            Solar
                          </text>
                        </motion.g>
                      ))}
                    </motion.g>
                  )}

                  {/* Biogas */}
                  {activeLayers.includes('biogas') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      <motion.circle
                        cx="75"
                        cy="40"
                        r="4"
                        fill={layers[2].color}
                        opacity="0.15"
                        animate={{ r: [4, 5, 4], opacity: [0.15, 0.25, 0.15] }}
                        transition={{ repeat: Infinity, duration: 4 }}
                      />
                      <circle cx="75" cy="40" r="1.5" fill={layers[2].color} opacity="0.5" />
                      <text x="75" y="46" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[2].color }}>
                        Biogas Facility
                      </text>
                    </motion.g>
                  )}

                  {/* BRT Transit */}
                  {activeLayers.includes('transit') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      <motion.path
                        d="M 12 33 Q 35 29 55 31 Q 75 33 90 36"
                        fill="none"
                        stroke={layers[3].color}
                        strokeWidth="0.8"
                        initial={{ pathLength: 0 }}
                        animate={{ pathLength: 1 }}
                        transition={{ duration: 1.5 }}
                      />
                      {[15, 28, 40, 52, 62, 72, 80, 87].map((x, i) => (
                        <motion.circle
                          key={`station-${i}`}
                          cx={x}
                          cy={30 + (x - 15) * 0.08}
                          r="1"
                          fill={layers[3].color}
                          initial={{ scale: 0 }}
                          animate={{ scale: 1 }}
                          transition={{ delay: 0.2 + i * 0.1 }}
                        />
                      ))}
                      <text x="50" y="37" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[3].color }}>
                        BRT Corridor
                      </text>
                    </motion.g>
                  )}

                  {/* Bike Infrastructure */}
                  {activeLayers.includes('bike') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      <motion.path
                        d="M 20 40 Q 35 38 50 42 Q 65 46 80 44"
                        fill="none"
                        stroke={layers[4].color}
                        strokeWidth="0.5"
                        strokeDasharray="1.5 0.8"
                        initial={{ pathLength: 0 }}
                        animate={{ pathLength: 1 }}
                        transition={{ duration: 2 }}
                      />
                      <text x="50" y="46" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[4].color }}>
                        Bike Network
                      </text>
                    </motion.g>
                  )}

                  {/* Community Development */}
                  {activeLayers.includes('community') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      {[{ x: 35, y: 35 }, { x: 55, y: 38 }, { x: 45, y: 45 }].map((pos, i) => (
                        <motion.g key={`comm-${i}`}>
                          <motion.rect
                            x={pos.x - 2}
                            y={pos.y - 2}
                            width="4"
                            height="4"
                            rx="1"
                            fill="none"
                            stroke={layers[5].color}
                            strokeWidth="0.3"
                            animate={{ opacity: [0.4, 0.8, 0.4] }}
                            transition={{ repeat: Infinity, duration: 3, delay: i * 0.7 }}
                          />
                          <circle cx={pos.x} cy={pos.y} r="0.6" fill={layers[5].color} opacity="0.6" />
                        </motion.g>
                      ))}
                      <text x="45" y="50" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[5].color }}>
                        Community Centers
                      </text>
                    </motion.g>
                  )}

                  {/* Green Housing */}
                  {activeLayers.includes('housing') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      {[{ x: 25, y: 30 }, { x: 40, y: 25 }, { x: 60, y: 35 }, { x: 50, y: 50 }].map((pos, i) => (
                        <motion.g key={`housing-${i}`}>
                          <rect
                            x={pos.x - 1.5}
                            y={pos.y - 1.5}
                            width="3"
                            height="3"
                            fill={layers[6].color}
                            opacity="0.25"
                            rx="0.3"
                          />
                        </motion.g>
                      ))}
                      <text x="25" y="35" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[6].color }}>
                        Housing
                      </text>
                    </motion.g>
                  )}

                  {/* Green Jobs */}
                  {activeLayers.includes('jobs') && (
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      transition={{ duration: 0.5 }}
                    >
                      <motion.circle
                        cx="40"
                        cy="32"
                        r="6"
                        fill="none"
                        stroke={layers[7].color}
                        strokeWidth="0.3"
                        strokeDasharray="1 0.5"
                        animate={{ r: [6, 8, 6], opacity: [0.3, 0.1, 0.3] }}
                        transition={{ repeat: Infinity, duration: 4 }}
                      />
                      <circle cx="40" cy="32" r="1.5" fill={layers[7].color} opacity="0.3" />
                      <text x="40" y="40" textAnchor="middle" className="font-mono" style={{ fontSize: '1.3px', fill: layers[7].color }}>
                        Training Center
                      </text>
                    </motion.g>
                  )}

                  {/* District label */}
                  <text x="50" y="12" textAnchor="middle" className="fill-white/20 font-serif" style={{ fontSize: '3px' }}>
                    New Orleans East
                  </text>
                </svg>
              </div>

              {/* Active layers indicator */}
              <div className="mt-4 pt-4 border-t border-white/5 flex flex-wrap gap-2">
                <span className="font-mono text-xs text-titanium/40 mr-2 self-center">Active:</span>
                {activeLayerData.length === 0 ? (
                  <span className="font-mono text-xs text-titanium/40 italic">No layers selected</span>
                ) : (
                  activeLayerData.map((layer) => (
                    <span
                      key={layer.id}
                      className="inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-xs font-mono"
                      style={{
                        backgroundColor: `${layer.color}15`,
                        color: layer.color,
                        borderWidth: '1px',
                        borderColor: `${layer.color}30`,
                      }}
                    >
                      <span
                        className="w-1.5 h-1.5 rounded-full"
                        style={{ backgroundColor: layer.color }}
                      />
                      {layer.name}
                    </span>
                  ))
                )}
              </div>
            </motion.div>
          </div>

          {/* Selected layer detail panel */}
          {selectedLayerData && (
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.4 }}
              className="mt-6 glass rounded-xl p-6 md:p-8"
              style={{ borderColor: `${selectedLayerData.color}30` }}
            >
              <div className="flex items-start justify-between mb-4">
                <div className="flex items-center gap-3">
                  <div
                    className="w-4 h-4 rounded-sm"
                    style={{ backgroundColor: selectedLayerData.color }}
                  />
                  <h3 className="font-serif text-xl text-white">{selectedLayerData.name}</h3>
                </div>
                <button
                  onClick={() => setSelectedLayer(null)}
                  className="text-titanium/40 hover:text-white text-sm font-mono transition-colors"
                >
                  close
                </button>
              </div>
              <p className="text-titanium text-sm leading-relaxed mb-6">{selectedLayerData.description}</p>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                {selectedLayerData.features.map((feature, i) => (
                  <motion.div
                    key={feature.label}
                    initial={{ opacity: 0, y: 10 }}
                    animate={{ opacity: 1, y: 0 }}
                    transition={{ duration: 0.3, delay: i * 0.08 }}
                    className="bg-white/[0.03] rounded-lg p-4"
                  >
                    <span className="text-white text-sm font-medium block">{feature.label}</span>
                    <span className="text-titanium/60 text-xs mt-1 block">{feature.detail}</span>
                  </motion.div>
                ))}
              </div>
            </motion.div>
          )}
        </div>
      </section>

      {/* All Layers Grid */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={detailView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={detailView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Plan Components
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Every layer, connected.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {layers.map((layer, i) => (
              <motion.button
                key={layer.id}
                initial={{ opacity: 0, y: 30 }}
                animate={detailView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.08 }}
                onClick={() => {
                  if (!activeLayers.includes(layer.id)) toggleLayer(layer.id)
                  setSelectedLayer(layer.id)
                  if (mapView.ref.current) {
                    window.scrollTo({ top: mapView.ref.current.offsetTop - 100, behavior: 'smooth' })
                  }
                }}
                className="glass rounded-xl p-5 text-left hover:bg-white/10 transition-all duration-300 group"
              >
                <div
                  className="w-3 h-3 rounded-sm mb-3"
                  style={{ backgroundColor: layer.color }}
                />
                <h3 className="font-serif text-sm text-white mb-2 group-hover:text-copper transition-colors">
                  {layer.name}
                </h3>
                <p className="text-titanium/60 text-xs leading-relaxed line-clamp-3">
                  {layer.description}
                </p>
              </motion.button>
            ))}
          </div>
        </div>
      </section>

      {/* Closing */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5">
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              whileInView={{ opacity: 1, y: 0 }}
              viewport={{ once: true }}
              transition={{ duration: 0.6 }}
              className="text-white text-lg font-serif leading-relaxed"
            >
              New Orleans East deserves more than recovery. It deserves transformation. This plan was designed to prove that sustainability, equity, and economic development are not competing priorities — they are the same priority, expressed through different layers of the same vision. The C40 Award recognized that ambition. The work continues.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}

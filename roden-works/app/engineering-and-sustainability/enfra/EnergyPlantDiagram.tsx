'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'

const components = [
  {
    id: 'boiler',
    label: 'Boilers',
    x: 15,
    y: 20,
    width: 18,
    height: 22,
    color: '#B87333',
    description: 'Generate steam at high pressure for heating, sterilization (autoclaves), and humidification throughout the hospital. Steam is the lifeblood of hospital operations.',
  },
  {
    id: 'chiller',
    label: 'Chillers',
    x: 40,
    y: 20,
    width: 20,
    height: 22,
    color: '#2D5A45',
    description: 'Produce chilled water at 42-44°F for air conditioning and critical cooling of operating rooms, server rooms, and pharmaceutical storage.',
  },
  {
    id: 'cooling-tower',
    label: 'Cooling Towers',
    x: 67,
    y: 15,
    width: 18,
    height: 28,
    color: '#8A9BA8',
    description: 'Reject heat from the chilled water loop to the atmosphere through evaporative cooling. Essential for maintaining chiller efficiency.',
  },
  {
    id: 'generator',
    label: 'Emergency Generators',
    x: 15,
    y: 58,
    width: 18,
    height: 20,
    color: '#B87333',
    description: 'Diesel or natural gas backup generators that ensure uninterrupted power during grid outages. Life-critical for ICUs, ORs, and life support systems.',
  },
  {
    id: 'pumps',
    label: 'Pumping Systems',
    x: 40,
    y: 58,
    width: 20,
    height: 20,
    color: '#2D5A45',
    description: 'Variable-speed pumps that circulate steam condensate, chilled water, and hot water through underground distribution networks to every building.',
  },
  {
    id: 'bas',
    label: 'Building Automation',
    x: 67,
    y: 58,
    width: 18,
    height: 20,
    color: '#8A9BA8',
    description: 'The BAS monitors and controls all mechanical systems — adjusting setpoints, optimizing schedules, and detecting faults in real-time via ENFRA Connect®.',
  },
]

export default function EnergyPlantDiagram() {
  const { ref, isInView } = useInView(0.1)
  const [activeComponent, setActiveComponent] = useState<string | null>(null)

  const active = components.find((c) => c.id === activeComponent)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Interactive Diagram
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Anatomy of a Central Energy Plant.
          </h2>
          <p className="mt-4 text-titanium max-w-2xl">
            The &ldquo;heart and lungs&rdquo; of a hospital — producing steam, chilled water, hot water, and electricity. Click each component to learn more.
          </p>
        </motion.div>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Diagram */}
          <motion.div
            initial={{ opacity: 0 }}
            animate={isInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.8, delay: 0.3 }}
            className="lg:col-span-2 glass rounded-xl p-6 md:p-8"
          >
            <div className="relative w-full" style={{ paddingBottom: '60%' }}>
              <svg
                viewBox="0 0 100 80"
                className="absolute inset-0 w-full h-full"
                fill="none"
              >
                {/* Connection lines - energy flow paths */}
                <motion.path
                  d="M 33 31 L 40 31"
                  stroke="#2D5A45"
                  strokeWidth="0.5"
                  strokeDasharray="2 1"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5, delay: 0.8 }}
                />
                <motion.path
                  d="M 60 31 L 67 31"
                  stroke="#2D5A45"
                  strokeWidth="0.5"
                  strokeDasharray="2 1"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5, delay: 1.0 }}
                />
                <motion.path
                  d="M 24 42 L 24 58"
                  stroke="#B87333"
                  strokeWidth="0.5"
                  strokeDasharray="2 1"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5, delay: 1.2 }}
                />
                <motion.path
                  d="M 50 42 L 50 58"
                  stroke="#2D5A45"
                  strokeWidth="0.5"
                  strokeDasharray="2 1"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5, delay: 1.4 }}
                />
                <motion.path
                  d="M 76 43 L 76 58"
                  stroke="#8A9BA8"
                  strokeWidth="0.5"
                  strokeDasharray="2 1"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5, delay: 1.6 }}
                />

                {/* Flow label */}
                <text x="50" y="52" textAnchor="middle" className="fill-titanium/30 text-[2.5px] font-mono">
                  UNDERGROUND DISTRIBUTION
                </text>

                {/* Components */}
                {components.map((comp, i) => (
                  <motion.g
                    key={comp.id}
                    initial={{ opacity: 0, scale: 0.9 }}
                    animate={isInView ? { opacity: 1, scale: 1 } : {}}
                    transition={{ duration: 0.5, delay: 0.5 + i * 0.1 }}
                    onClick={() => setActiveComponent(activeComponent === comp.id ? null : comp.id)}
                    className="cursor-pointer"
                  >
                    <rect
                      x={comp.x}
                      y={comp.y}
                      width={comp.width}
                      height={comp.height}
                      rx="1.5"
                      fill={activeComponent === comp.id ? comp.color + '30' : 'rgba(255,255,255,0.03)'}
                      stroke={activeComponent === comp.id ? comp.color : 'rgba(255,255,255,0.1)'}
                      strokeWidth={activeComponent === comp.id ? '0.8' : '0.4'}
                      className="transition-all duration-300"
                    />
                    {/* Component icon area */}
                    <circle
                      cx={comp.x + comp.width / 2}
                      cy={comp.y + comp.height / 2 - 2}
                      r="3"
                      fill={comp.color + '20'}
                      stroke={comp.color + '60'}
                      strokeWidth="0.3"
                    />
                    <text
                      x={comp.x + comp.width / 2}
                      y={comp.y + comp.height - 3}
                      textAnchor="middle"
                      className="fill-white text-[2.5px] font-sans"
                    >
                      {comp.label}
                    </text>
                  </motion.g>
                ))}

                {/* Title */}
                <text x="50" y="8" textAnchor="middle" className="fill-white/40 text-[3px] font-serif">
                  Central Energy Plant — Schematic
                </text>
              </svg>
            </div>
          </motion.div>

          {/* Info panel */}
          <motion.div
            initial={{ opacity: 0, x: 20 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="glass rounded-xl p-6"
          >
            {active ? (
              <motion.div
                key={active.id}
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.3 }}
              >
                <div
                  className="w-3 h-3 rounded-full mb-4"
                  style={{ backgroundColor: active.color }}
                />
                <h3 className="font-serif text-xl text-white mb-3">{active.label}</h3>
                <p className="text-titanium text-sm leading-relaxed">{active.description}</p>
              </motion.div>
            ) : (
              <div className="h-full flex flex-col items-center justify-center text-center py-8">
                <div className="w-12 h-12 rounded-full bg-white/5 flex items-center justify-center mb-4">
                  <svg className="w-6 h-6 text-titanium/40" fill="none" stroke="currentColor" viewBox="0 0 24 24" strokeWidth="1.5">
                    <path strokeLinecap="round" strokeLinejoin="round" d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.227 7.917-3.286-.672zM12 2.25V4.5m5.834.166l-1.591 1.591M20.25 10.5H18M7.757 14.743l-1.59 1.591M6 10.5H3.75m4.007-4.243l-1.59-1.591" />
                  </svg>
                </div>
                <p className="text-titanium/60 text-sm">
                  Click on a component in the diagram to learn about its role in hospital infrastructure.
                </p>
              </div>
            )}
          </motion.div>
        </div>
      </div>
    </section>
  )
}

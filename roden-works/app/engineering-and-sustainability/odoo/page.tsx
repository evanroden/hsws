'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StatCounter from '@/components/ui/StatCounter'

const modules = [
  {
    id: 'mrp',
    label: 'MRP',
    x: 40,
    y: 12,
    width: 22,
    height: 14,
    color: '#B87333',
    description:
      'Manufacturing Resource Planning — multi-level bills of materials, work center routing, Master Production Schedule, finite capacity planning, and OEE analysis. Evan implemented MRP for discrete manufacturers transitioning from spreadsheet-based production tracking to integrated workflows with real-time shop floor visibility.',
  },
  {
    id: 'inventory',
    label: 'Inventory',
    x: 10,
    y: 35,
    width: 22,
    height: 14,
    color: '#2D5A45',
    description:
      'Real-time warehouse management with barcode scanning, automated replenishment rules, multi-location tracking, lot/serial traceability, and putaway strategies. Inventory connects directly to MRP for demand-driven procurement and to Sales for accurate delivery promises.',
  },
  {
    id: 'accounting',
    label: 'Accounting',
    x: 68,
    y: 35,
    width: 22,
    height: 14,
    color: '#8A9BA8',
    description:
      'Analytic accounting with parallel ledger for internal cost tracking, percentage-based distribution across departments, automated bank reconciliation, and multi-currency support. Evan specialized in analytic accounting implementations that gave CFOs visibility into profitability by product line, project, or department.',
  },
  {
    id: 'sales',
    label: 'Sales',
    x: 10,
    y: 58,
    width: 22,
    height: 14,
    color: '#B87333',
    description:
      'Full CRM and pipeline management, configurable quotation templates, subscription management, and e-commerce integration. The Sales module drives upstream demand signals to Inventory and MRP while Accounting auto-generates invoices on delivery confirmation.',
  },
  {
    id: 'portal',
    label: 'Customer Portal',
    x: 68,
    y: 58,
    width: 22,
    height: 14,
    color: '#2D5A45',
    description:
      'Self-service portal for order tracking, invoice payment, support ticket submission, and document sharing. Reduces operational overhead by empowering customers to manage their own accounts — a key expansion revenue driver in food & beverage and retail implementations.',
  },
]

const nrrBreakdown = [
  {
    label: 'Starting ARR',
    value: '$100',
    width: '62.5%',
    color: 'bg-titanium/40',
    note: 'Annual recurring revenue at period start',
  },
  {
    label: 'End-of-Period ARR',
    value: '$160',
    width: '100%',
    color: 'bg-gradient-to-r from-copper to-copper/60',
    note: 'After expansion, upsells, and retention',
  },
  {
    label: 'Industry Median',
    value: '~$110',
    width: '68.75%',
    color: 'bg-titanium/20',
    note: 'Typical SaaS NRR is 103-114%',
  },
]

export default function OdooPage() {
  const { ref: aboutRef, isInView: aboutInView } = useInView(0.1)
  const { ref: nrrRef, isInView: nrrInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)
  const [activeModule, setActiveModule] = useState<string | null>(null)

  const active = modules.find((m) => m.id === activeModule)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'Odoo' },
        ]}
      />

      {/* Hero Section */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
        {/* Animated module grid background */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.04]"
            viewBox="0 0 1200 600"
          >
            {Array.from({ length: 5 }).map((_, row) =>
              Array.from({ length: 8 }).map((_, col) => (
                <motion.rect
                  key={`${row}-${col}`}
                  x={50 + col * 140}
                  y={50 + row * 110}
                  width="100"
                  height="70"
                  rx="8"
                  fill="none"
                  stroke="#B87333"
                  strokeWidth="0.5"
                  initial={{ opacity: 0 }}
                  animate={{ opacity: [0, 0.6, 0] }}
                  transition={{
                    repeat: Infinity,
                    duration: 3,
                    delay: (row + col) * 0.3,
                    ease: 'easeInOut',
                  }}
                />
              ))
            )}
          </svg>
        </div>

        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Enterprise Resource Planning
            </span>
            <h1 className="font-serif text-display text-white max-w-3xl">
              Odoo
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              The world&apos;s most installed open-source ERP with 13M+ users
              worldwide. Backed by CapitalG, Sequoia, and BlackRock at a
              valuation exceeding &euro;5 billion.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-3 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <StatCounter value={160} suffix="%" label="Net Revenue Retention" />
            <StatCounter value={13} suffix="M+" label="Global Users" />
            <StatCounter value={5} prefix="€" suffix="B" label="Valuation" />
          </motion.div>
        </div>
      </section>

      {/* About Section */}
      <section className="section-padding bg-slate-950" ref={aboutRef}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={aboutInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                About Odoo
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                Open-source ERP for the modern enterprise.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Odoo is the world&apos;s most installed open-source ERP
                  platform, serving 13 million+ users across 180+ countries.
                  Unlike monolithic ERPs like SAP or Oracle, Odoo&apos;s modular
                  architecture allows businesses to start with a single
                  application — CRM, Accounting, Inventory — and expand
                  organically as needs evolve.
                </p>
                <p>
                  Founded in Belgium in 2005 by Fabien Pinckaers, Odoo has grown
                  to a &euro;5B+ valuation with backing from CapitalG
                  (Alphabet&apos;s investment arm), Sequoia Capital, and
                  BlackRock. The platform includes 82 official modules and
                  50,000+ community apps, covering everything from manufacturing
                  and accounting to point-of-sale and website building.
                </p>
                <p>
                  Odoo&apos;s dual-licensing model — Community (open-source) and
                  Enterprise (subscription) — creates a powerful land-and-expand
                  motion. Clients begin with free tools, prove value, then
                  upgrade for advanced features like analytic accounting, barcode
                  scanning, and IoT integration.
                </p>
              </div>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={aboutInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Role
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                Account Executive
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Account Executive (February 2024 &ndash; February 2025)
                  managing full software implementation cycles for Odoo&apos;s
                  ERP platform. Specialized in MRP for discrete manufacturing,
                  analytic accounting, food &amp; beverage software, and
                  retail/customer-portal implementations.
                </p>
                <p>
                  Achieved 160% net revenue retention — exceptional performance
                  in an industry where the median is 103&ndash;114%. Even elite
                  SaaS companies like Snowflake (~130%) and Twilio (~120%) rarely
                  reach this threshold. This figure reflects not just retention,
                  but significant expansion and upselling within the existing
                  client base.
                </p>
              </div>

              <div className="mt-8 glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Specializations
                </h3>
                <ul className="space-y-2 text-sm text-titanium">
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Manufacturing Resource Planning (MRP) for discrete
                    manufacturers
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Analytic accounting with multi-dimensional cost tracking
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Food &amp; beverage operations (POS, inventory, kitchen
                    display)
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Retail &amp; e-commerce with customer portal integration
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Data migration from legacy systems (QuickBooks, Excel, Sage)
                  </li>
                </ul>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* NRR Explainer */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-copper/5"
        ref={nrrRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={nrrInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Performance
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              How $100 becomes $160.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Net Revenue Retention measures how much revenue you keep and grow
              from existing customers. An NRR of 160% means that for every $100
              of starting recurring revenue, the book grew to $160 — after
              accounting for churn, contraction, and expansion.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {nrrBreakdown.map((item, i) => (
              <motion.div
                key={item.label}
                initial={{ opacity: 0, y: 20 }}
                animate={nrrInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className={`glass rounded-xl p-6 text-center ${
                  i === 1 ? 'border-copper/20' : ''
                }`}
              >
                <span
                  className={`font-serif text-3xl ${
                    i === 1 ? 'text-copper' : i === 2 ? 'text-titanium/60' : 'text-white'
                  }`}
                >
                  {item.value}
                </span>
                <span className="block text-titanium text-sm mt-2">
                  {item.label}
                </span>
                <div className="mt-4 h-2 bg-white/10 rounded-full overflow-hidden">
                  <motion.div
                    initial={{ width: 0 }}
                    animate={nrrInView ? { width: item.width } : {}}
                    transition={{ duration: 1, delay: 0.5 + i * 0.2 }}
                    className={`h-full ${item.color} rounded-full`}
                  />
                </div>
                <span className="text-xs text-titanium/50 mt-2 block">
                  {item.note}
                </span>
              </motion.div>
            ))}
          </div>

          {/* Context benchmarks */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={nrrInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.8 }}
            className="mt-8 glass rounded-xl p-6"
          >
            <h3 className="font-serif text-lg text-white mb-4">
              Industry context
            </h3>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              {[
                { company: 'Evan @ Odoo', nrr: '160%', highlight: true },
                { company: 'Snowflake', nrr: '~131%', highlight: false },
                { company: 'Twilio', nrr: '~120%', highlight: false },
                { company: 'SaaS Median', nrr: '~110%', highlight: false },
              ].map((bench) => (
                <div
                  key={bench.company}
                  className={`rounded-lg p-4 text-center ${
                    bench.highlight
                      ? 'bg-copper/10 border border-copper/20'
                      : 'bg-white/5'
                  }`}
                >
                  <span
                    className={`font-serif text-xl ${
                      bench.highlight ? 'text-copper' : 'text-titanium'
                    }`}
                  >
                    {bench.nrr}
                  </span>
                  <span className="block text-xs text-titanium/60 mt-1 font-mono">
                    {bench.company}
                  </span>
                </div>
              ))}
            </div>
          </motion.div>
        </div>
      </section>

      {/* Interactive ERP Module Diagram */}
      <section className="section-padding bg-slate-950" ref={diagramRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Architecture
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              ERP module ecosystem.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Odoo&apos;s power lies in integration. Every module connects
              natively — a sales order triggers inventory reservation, which
              feeds MRP scheduling, which generates purchase orders, which flow
              into accounting. Click each module to explore.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
            {/* SVG Diagram */}
            <motion.div
              initial={{ opacity: 0 }}
              animate={diagramInView ? { opacity: 1 } : {}}
              transition={{ duration: 0.8, delay: 0.3 }}
              className="lg:col-span-2 glass rounded-xl p-6 md:p-8"
            >
              <div className="relative w-full" style={{ paddingBottom: '60%' }}>
                <svg
                  viewBox="0 0 100 80"
                  className="absolute inset-0 w-full h-full"
                  fill="none"
                >
                  {/* Title */}
                  <text
                    x="50"
                    y="6"
                    textAnchor="middle"
                    className="fill-white/40 text-[3px] font-serif"
                  >
                    Odoo ERP Module Architecture
                  </text>

                  {/* Connection lines — data flow */}
                  {/* MRP ↔ Inventory */}
                  <motion.path
                    d="M 35 19 L 25 35"
                    stroke="#2D5A45"
                    strokeWidth="0.4"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 0.8 }}
                  />
                  {/* MRP ↔ Accounting */}
                  <motion.path
                    d="M 55 19 L 75 35"
                    stroke="#8A9BA8"
                    strokeWidth="0.4"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 1.0 }}
                  />
                  {/* Inventory ↔ Sales */}
                  <motion.path
                    d="M 21 49 L 21 58"
                    stroke="#B87333"
                    strokeWidth="0.4"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 1.2 }}
                  />
                  {/* Accounting ↔ Portal */}
                  <motion.path
                    d="M 79 49 L 79 58"
                    stroke="#2D5A45"
                    strokeWidth="0.4"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 1.4 }}
                  />
                  {/* Sales ↔ Portal (horizontal) */}
                  <motion.path
                    d="M 32 65 L 68 65"
                    stroke="#B87333"
                    strokeWidth="0.4"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 1.6 }}
                  />
                  {/* Inventory ↔ Accounting (horizontal) */}
                  <motion.path
                    d="M 32 42 L 68 42"
                    stroke="#8A9BA8"
                    strokeWidth="0.3"
                    strokeDasharray="1 0.5"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.2, delay: 1.8 }}
                  />

                  {/* Flow labels */}
                  <text
                    x="50"
                    y="30"
                    textAnchor="middle"
                    className="fill-titanium/20 text-[2px] font-mono"
                  >
                    DATA FLOW
                  </text>

                  {/* Modules */}
                  {modules.map((mod, i) => (
                    <motion.g
                      key={mod.id}
                      initial={{ opacity: 0, scale: 0.9 }}
                      animate={
                        diagramInView ? { opacity: 1, scale: 1 } : {}
                      }
                      transition={{
                        duration: 0.5,
                        delay: 0.5 + i * 0.1,
                      }}
                      onClick={() =>
                        setActiveModule(
                          activeModule === mod.id ? null : mod.id
                        )
                      }
                      className="cursor-pointer"
                    >
                      <rect
                        x={mod.x}
                        y={mod.y}
                        width={mod.width}
                        height={mod.height}
                        rx="2"
                        fill={
                          activeModule === mod.id
                            ? mod.color + '30'
                            : 'rgba(255,255,255,0.03)'
                        }
                        stroke={
                          activeModule === mod.id
                            ? mod.color
                            : 'rgba(255,255,255,0.1)'
                        }
                        strokeWidth={
                          activeModule === mod.id ? '0.8' : '0.4'
                        }
                        className="transition-all duration-300"
                      />
                      <circle
                        cx={mod.x + mod.width / 2}
                        cy={mod.y + mod.height / 2 - 1}
                        r="2.5"
                        fill={mod.color + '20'}
                        stroke={mod.color + '60'}
                        strokeWidth="0.3"
                      />
                      <text
                        x={mod.x + mod.width / 2}
                        y={mod.y + mod.height - 2}
                        textAnchor="middle"
                        className="fill-white text-[2.8px] font-mono"
                      >
                        {mod.label}
                      </text>
                    </motion.g>
                  ))}
                </svg>
              </div>
            </motion.div>

            {/* Info Panel */}
            <motion.div
              initial={{ opacity: 0, x: 20 }}
              animate={diagramInView ? { opacity: 1, x: 0 } : {}}
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
                  <h3 className="font-serif text-xl text-white mb-3">
                    {active.label}
                  </h3>
                  <p className="text-titanium text-sm leading-relaxed">
                    {active.description}
                  </p>
                </motion.div>
              ) : (
                <div className="h-full flex flex-col items-center justify-center text-center py-8">
                  <div className="w-12 h-12 rounded-full bg-white/5 flex items-center justify-center mb-4">
                    <svg
                      className="w-6 h-6 text-titanium/40"
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                      strokeWidth="1.5"
                    >
                      <path
                        strokeLinecap="round"
                        strokeLinejoin="round"
                        d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.227 7.917-3.286-.672zM12 2.25V4.5m5.834.166l-1.591 1.591M20.25 10.5H18M7.757 14.743l-1.59 1.591M6 10.5H3.75m4.007-4.243l-1.59-1.591"
                      />
                    </svg>
                  </div>
                  <p className="text-titanium/60 text-sm">
                    Click on a module in the diagram to see how Evan implemented
                    it for clients.
                  </p>
                </div>
              )}
            </motion.div>
          </div>
        </div>
      </section>
    </>
  )
}

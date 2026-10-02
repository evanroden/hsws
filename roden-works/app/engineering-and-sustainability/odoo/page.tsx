'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StatCounter from '@/components/ui/StatCounter'
import { BreadcrumbJsonLd, ArticleJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import GoalBulletChart from './GoalBulletChart'
import OrderToCashFlow from './OrderToCashFlow'

export default function OdooPage() {
  const { ref: aboutRef, isInView: aboutInView } = useInView(0.1)
  const { ref: nrrRef, isInView: nrrInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)

  return (
    <>
      <ArticleJsonLd
        title="Odoo: ERP Implementation for Manufacturing"
        description="My year selling and implementing Odoo ERP for manufacturing and distribution companies, including a month at 160% of my non-recurring revenue goal."
        path="/engineering-and-sustainability/odoo"
      />
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'Odoo' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'Odoo' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1100} />
      </div>

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

        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
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
              Open-source ERP software with 13M+ users worldwide, backed by
              CapitalG, Sequoia, and BlackRock at a valuation above &euro;5
              billion.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-3 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <StatCounter value={160} suffix="%" label="Non-Recurring Revenue Goal" />
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
                Modular, open-source ERP.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Odoo is an open-source ERP platform with more than 13 million
                  users in 180+ countries. Compared with SAP or Oracle, it is
                  modular: a business can start with one application, such as
                  CRM, Accounting, or Inventory, and add others later.
                </p>
                <p>
                  Founded in Belgium in 2005 by Fabien Pinckaers, Odoo has grown
                  to a &euro;5B+ valuation with backing from CapitalG
                  (Alphabet&apos;s investment arm), Sequoia Capital, and
                  BlackRock. The platform includes 82 official modules and
                  50,000+ community apps for manufacturing, accounting,
                  point-of-sale, website building, and more.
                </p>
                <p>
                  Odoo has two editions: Community (open-source) and Enterprise
                  (subscription). Many clients start on the free tools and
                  upgrade once they need features like analytic accounting,
                  barcode scanning, or IoT integration.
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
                  From February 2024 to February 2025 I was an Account
                  Executive at Odoo, managing full implementation cycles for
                  clients. Most of my work was MRP for discrete manufacturing,
                  analytic accounting, food &amp; beverage software, and
                  retail/customer-portal implementations.
                </p>
                <p>
                  In one month I hit 160% of my non-recurring revenue goal,
                  mostly from upsells and new module implementations with
                  existing clients.
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

      {/* Revenue Performance */}
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
              160% of goal.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              My best month at Odoo came in at 160% of my non-recurring
              revenue target. Most of it came from upsells and new modules for
              clients I already had.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={nrrInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.15 }}
            className="max-w-4xl"
          >
            <GoalBulletChart animate={nrrInView} />
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
              How the modules connect.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              In Odoo, each module hands work to the next. A sales order reserves
              inventory, a shortage becomes an MRP manufacturing order, MRP
              generates purchase orders, and those flow into accounting. Click
              a module for details.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.15 }}
          >
            <OrderToCashFlow />
          </motion.div>
        </div>
      </section>
      <ProjectNav currentSlug="odoo" />
    </>
  )
}

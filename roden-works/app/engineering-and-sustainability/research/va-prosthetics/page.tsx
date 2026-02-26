'use client'

import { motion } from 'framer-motion'
import dynamic from 'next/dynamic'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

const ProstheticViewer = dynamic(
  () => import('@/components/three/ProstheticViewer'),
  {
    ssr: false,
    loading: () => (
      <div className="glass rounded-xl aspect-square flex items-center justify-center">
        <div className="flex flex-col items-center gap-3">
          <div className="w-8 h-8 border-2 border-copper/30 border-t-copper rounded-full animate-spin" />
          <span className="font-mono text-xs text-titanium">
            Loading 3D viewer...
          </span>
        </div>
      </div>
    ),
  }
)

export default function VAProstheticsPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: viewerRef, isInView: viewerInView } = useInView(0.1)
  const { ref: processRef, isInView: processInView } = useInView(0.1)
  const { ref: stackRef, isInView: stackInView } = useInView(0.1)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          {
            label: 'Research',
            href: '/engineering-and-sustainability/research',
          },
          { label: 'VA Prosthetics' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
        {/* Subtle geometric pattern */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.03]"
            viewBox="0 0 1200 600"
          >
            {Array.from({ length: 5 }).map((_, i) => (
              <motion.path
                key={i}
                d={`M ${200 + i * 200} 100 L ${250 + i * 200} 200 L ${150 + i * 200} 200 Z`}
                fill="none"
                stroke="#2D5A45"
                strokeWidth="1"
                initial={{ pathLength: 0 }}
                animate={{ pathLength: 1 }}
                transition={{ duration: 2, delay: i * 0.3 }}
              />
            ))}
          </svg>
        </div>

        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Biomedical Engineering
            </span>
            <h1 className="font-serif text-display text-white max-w-3xl">
              VA Prosthetics
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Designing and 3D-printing custom prosthetic devices that restore
              autonomy to American veterans — one device, one person at a time.
            </p>
          </motion.div>
        </div>
      </section>

      {/* About + 3D Viewer */}
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            {/* Content */}
            <div ref={contentRef}>
              <motion.div
                initial={{ opacity: 0, y: 30 }}
                animate={contentInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6 }}
              >
                <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                  Sept 2022 &ndash; Jan 2025
                </span>
                <h2 className="font-serif text-heading text-white mb-6">
                  Biomedical Engineer &amp; Project Manager
                </h2>
                <div className="space-y-4 text-titanium leading-relaxed">
                  <p>
                    A partnership between Tulane University and the U.S.
                    Department of Veterans Affairs under the Taylor Foundation.
                    Evan modeled custom medical devices in Autodesk Fusion 360
                    and FlowIt for 3D printing, building prosthetic devices for
                    American veterans in the Southern Louisiana region.
                  </p>
                  <p>
                    Each device is unique — designed around the specific anatomy,
                    lifestyle, and functional needs of the individual veteran.
                    The process begins with clinical assessment and 3D scanning,
                    moves through iterative CAD modeling, and culminates in a
                    printed device that is fitted, tested, and refined until it
                    works for the person it was built for.
                  </p>
                  <p>
                    The project operated at the intersection of biomedical
                    engineering and direct patient care, requiring both technical
                    fluency in additive manufacturing and the interpersonal
                    sensitivity to work with veterans navigating limb loss and
                    mobility challenges.
                  </p>
                </div>

                {/* Emotional framing */}
                <motion.div
                  initial={{ opacity: 0 }}
                  animate={contentInView ? { opacity: 1 } : {}}
                  transition={{ duration: 0.8, delay: 0.4 }}
                  className="mt-8 glass rounded-xl p-6 border-l-2 border-copper/40"
                >
                  <p className="text-white italic font-serif text-lg leading-relaxed">
                    &ldquo;This is not clinical work in the abstract. Every
                    device represents a person regaining something they lost —
                    the ability to grip a coffee cup, to reach a shelf, to live
                    without depending on someone else for basic tasks. Engineering
                    has never felt more personal.&rdquo;
                  </p>
                </motion.div>
              </motion.div>
            </div>

            {/* Interactive 3D Viewer */}
            <div ref={viewerRef}>
              <motion.div
                initial={{ opacity: 0, scale: 0.95 }}
                animate={viewerInView ? { opacity: 1, scale: 1 } : {}}
                transition={{ duration: 0.6 }}
                className="glass rounded-xl overflow-hidden"
              >
                <ProstheticViewer
                  models={[
                    { path: '/models/va-dent-1.glb', label: 'Device 1' },
                    { path: '/models/va-dent-2.glb', label: 'Device 2' },
                  ]}
                  className="aspect-square"
                />
              </motion.div>
            </div>
          </div>
        </div>
      </section>

      {/* Design Process */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={processRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={processInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Process
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              From assessment to autonomy.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Every device follows a structured design process — but flexibility
              is built in at every step, because no two veterans have the same
              needs.
            </p>
          </motion.div>

          <div className="relative">
            <div className="hidden md:block absolute top-12 left-0 right-0 h-px bg-white/10" />

            <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
              {[
                {
                  step: '01',
                  title: 'Clinical Assessment',
                  description:
                    'Work directly with the veteran and clinical team to understand functional goals, anatomy, and lifestyle requirements.',
                },
                {
                  step: '02',
                  title: '3D Modeling',
                  description:
                    'Design the device in Autodesk Fusion 360, iterating through parametric models informed by anatomical measurements and biomechanical analysis.',
                },
                {
                  step: '03',
                  title: 'Fabrication',
                  description:
                    'Print using FlowIt adaptive manufacturing (FDM/SLA), selecting materials for strength, flexibility, and biocompatibility.',
                },
                {
                  step: '04',
                  title: 'Fitting & Refinement',
                  description:
                    'Fit the printed device to the veteran, test functionality, and refine through iterative adjustments until it meets their daily needs.',
                },
              ].map((phase, i) => (
                <motion.div
                  key={phase.step}
                  initial={{ opacity: 0, y: 30 }}
                  animate={processInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: i * 0.12 }}
                  className="relative"
                >
                  <div className="flex items-center gap-3 mb-6">
                    <motion.div
                      initial={{ scale: 0 }}
                      animate={processInView ? { scale: 1 } : {}}
                      transition={{
                        duration: 0.4,
                        delay: 0.3 + i * 0.12,
                      }}
                      className="w-8 h-8 rounded-full bg-forest/20 border border-forest-light/40 flex items-center justify-center"
                    >
                      <span className="font-mono text-xs text-forest-light">
                        {phase.step}
                      </span>
                    </motion.div>
                  </div>
                  <div className="glass rounded-xl p-5">
                    <h3 className="font-serif text-lg text-white mb-2">
                      {phase.title}
                    </h3>
                    <p className="text-titanium text-sm leading-relaxed">
                      {phase.description}
                    </p>
                  </div>
                </motion.div>
              ))}
            </div>
          </div>
        </div>
      </section>

      {/* Technical Stack */}
      <section className="section-padding bg-slate-950" ref={stackRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={stackInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-8"
          >
            <h2 className="font-serif text-heading text-white">
              Technical Stack
            </h2>
          </motion.div>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {[
              {
                tool: 'Autodesk Fusion 360',
                detail: 'Parametric CAD modeling',
              },
              {
                tool: 'FlowIt',
                detail: 'Adaptive 3D print slicing',
              },
              {
                tool: '3D Printing (FDM/SLA)',
                detail: 'PLA, PETG, flexible TPU',
              },
              {
                tool: 'Clinical Assessment',
                detail: 'Anatomical measurement & fitting',
              },
            ].map((item, i) => (
              <motion.div
                key={item.tool}
                initial={{ opacity: 0, y: 20 }}
                animate={stackInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.1 }}
                className="glass rounded-lg p-4 text-center"
              >
                <span className="text-white text-sm block">{item.tool}</span>
                <span className="text-titanium/50 text-xs font-mono mt-1 block">
                  {item.detail}
                </span>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}

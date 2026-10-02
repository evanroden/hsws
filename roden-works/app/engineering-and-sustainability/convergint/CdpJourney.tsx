'use client'

import { motion, useReducedMotion } from 'framer-motion'
import type { ReactNode } from 'react'

export interface CdpStep {
  phase: string
  title: string
  location: string
  description: string
  /** Optional <Cite> rendered after the description. */
  cite?: ReactNode
}

/**
 * Horizontal stepper on desktop (numbered markers on a connecting progress
 * line), vertical stepper on mobile (markers on a left rail).
 */
export default function CdpJourney({ steps, animate }: { steps: CdpStep[]; animate: boolean }) {
  const reduceMotion = useReducedMotion()
  const go = animate || reduceMotion
  const n = steps.length

  return (
    <div className="relative">
      {/* Desktop rail: from the first marker's center to the last marker's center */}
      <div
        aria-hidden="true"
        className="hidden md:block absolute top-4 left-4 h-px bg-white/10"
        style={{ right: `calc((100% - ${(n - 1) * 2}rem) / ${n} - 1rem)` }}
      >
        <motion.div
          className="h-full origin-left bg-gradient-to-r from-copper to-copper/40"
          initial={{ scaleX: 0 }}
          animate={go ? { scaleX: 1 } : {}}
          transition={{ duration: reduceMotion ? 0 : 1.2, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
        />
      </div>
      {/* Mobile rail */}
      <div aria-hidden="true" className="md:hidden absolute left-4 top-4 bottom-4 w-px bg-white/10">
        <motion.div
          className="w-full h-full origin-top bg-gradient-to-b from-copper to-copper/40"
          initial={{ scaleY: 0 }}
          animate={go ? { scaleY: 1 } : {}}
          transition={{ duration: reduceMotion ? 0 : 1.2, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
        />
      </div>

      <ol className="relative grid grid-cols-1 md:grid-cols-3 gap-10 md:gap-8">
      {steps.map((step, i) => (
        <motion.li
          key={step.title}
          initial={{ opacity: 0, y: 20 }}
          animate={go ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: reduceMotion ? 0 : 0.6, delay: reduceMotion ? 0 : 0.2 + i * 0.15 }}
          className="relative flex gap-5 md:flex-col md:gap-0"
        >
          <div className="relative z-10 flex items-center gap-4 md:mb-6">
            <span className="flex h-8 w-8 shrink-0 items-center justify-center rounded-full border border-copper/60 bg-slate-950 font-sans text-sm font-semibold text-copper-light">
              {i + 1}
            </span>
            <span className="hidden md:inline bg-slate-950 pr-3 -ml-1 pl-1 font-mono text-xs tracking-wide uppercase text-muted">{step.phase}</span>
          </div>

          <div className="min-w-0 md:pr-4">
            <span className="md:hidden block font-mono text-xs tracking-wide uppercase text-muted mb-1.5 pt-1.5">
              {step.phase}
            </span>
            <h3 className="font-serif text-2xl text-white">{step.title}</h3>
            <span className="mt-1 block font-mono text-xs text-copper-light">{step.location}</span>
            <p className="mt-4 text-titanium text-sm leading-relaxed max-w-md">{step.description}{step.cite}</p>
          </div>
        </motion.li>
      ))}
      </ol>
    </div>
  )
}

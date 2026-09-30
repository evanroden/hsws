'use client'

import { motion } from 'framer-motion'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'

interface AnimatedCardProps {
  href: string
  title: string
  description: string
  label?: string
  index?: number
  children?: React.ReactNode
}

export default function AnimatedCard({
  href,
  title,
  description,
  label,
  index = 0,
  children,
}: AnimatedCardProps) {
  const { ref, isInView } = useInView(0.1)

  return (
    <motion.div
      ref={ref}
      className="h-full"
      initial={{ opacity: 0, y: 30 }}
      animate={isInView ? { opacity: 1, y: 0 } : {}}
      transition={{ duration: 0.6, delay: index * 0.1, ease: [0.16, 1, 0.3, 1] }}
    >
      <Link href={href} className="group block h-full">
        <div className="flex h-full flex-col rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-7 transition-all duration-300 group-hover:-translate-y-0.5 group-hover:border-white/20 group-hover:bg-surface-raised group-hover:shadow-2xl group-hover:shadow-black/30">
          {label && (
            <span className="inline-block font-mono text-[11px] tracking-[0.18em] uppercase text-copper-light mb-3">
              {label}
            </span>
          )}
          {children}
          <h3 className="font-serif text-xl md:text-2xl text-white mb-3">{title}</h3>
          <p className="text-titanium text-sm leading-relaxed flex-1">{description}</p>
          <div className="mt-6 flex items-center gap-2 text-sm font-medium text-muted group-hover:text-white transition-colors">
            <span>Explore</span>
            <span aria-hidden="true" className="inline-block transition-transform duration-300 group-hover:translate-x-1">
              &rarr;
            </span>
          </div>
        </div>
      </Link>
    </motion.div>
  )
}

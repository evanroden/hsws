import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import Timeline from './Timeline'
import CaseStudyGrid from './CaseStudyGrid'
import SkillsRadar from './SkillsRadar'
import { SourceList } from '@/components/ui/Sources'
import { ENGINEERING_SOURCES } from './sources'

export const metadata: Metadata = {
  title: 'Engineering & Sustainability — EaaS, Biomedical Research & Systems',
  description:
    'Hospital energy plants, building safety systems, ERP software, and biomedical research at Tulane.',
}

export default function EngineeringPage() {
  return (
    <>
      <PageHero
        title="Engineering & Sustainability"
        subtitle="Hospital energy plants, building safety systems, ERP software, and biomedical research at Tulane."
        label="Technical Portfolio"
        variant="dark"
      />
      <Timeline />
      <CaseStudyGrid />
      <SkillsRadar />
      <SourceList sources={ENGINEERING_SOURCES} />
    </>
  )
}

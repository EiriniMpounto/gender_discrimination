import statistics


class DescriptiveStatistics:
      
      def data_counts(data):
            '''
            The current function perfors simple data counts in 
            order to see how many women and men there are on 
            this dataset, or how many ceos there are on the dataset etc.
            '''
            sex_counts = data['GENDER'].value_counts()
            pceo_counts = data['PCEO'].value_counts()
            pcfo_counts = data['PCFO'].value_counts()
            ceo_counts = data['CEO_NEW'].value_counts()
            cfo_counts = data['CFO_NEW'].value_counts()
            # print results
            print('How many women and men we have on our dataset',
                  sex_counts)
            
            print('The number of the current CEOS in the dataset',
                  pceo_counts)
            
            print('The number of the current CFOs in the dataset', 
                  pcfo_counts)
            
            print('''The number of the executives that have had 
                  at least once the title of the CEO
                  ''', cfo_counts)
            
            print('''The number of executives that have had at
                  least once the title of the CEO: ''' , ceo_counts)
    
            return
      
      def statistical_calculations(data):
            ''' This function calculates the measurements 
            of central tendency and dispersion '''

            # spindex mode
            spindex_mode = data['SPINDEX'].mode()
            print('The mode for the SPINDEX variable is:', spindex_mode)
            # spcode mode
            spcode_mode = data['SPCODE'].mode()
            print('The mode of the SPCODE variable is:', spcode_mode)
            
            return 
      
      def gender_comparisons(data):
            '''
            The current function performs main data comparisons
            with the gender as a filter.
            '''
            # pceo and gender
            ceo_gender = data.groupby(
                  ['PCEO', 'GENDER'])['EXEC_FULLNAME']
            calculate_ceo_gender = ceo_gender.count()
            
            print('''How many women and men have
                  currently the title CEO''' , 
                  calculate_ceo_gender)
            
            # pcfo and gender
            cfo_gender = data.groupby(
                  ['PCFO', 'GENDER'])['EXEC_FULLNAME']
            calculate_cfo_gender = cfo_gender.count()
            
            print('''How many women and men have
                  currently the CFO title''', 
                  calculate_cfo_gender)
                  

            # ceo and gender
            ceo_gender = data.groupby(
                  ['CEO_NEW', 'GENDER'])['EXEC_FULLNAME']
            calculate_ceo_gender = ceo_gender.count()
            
            print('''How many women and men have had the 
                  title CEO at least once''',
                  calculate_ceo_gender)
            
            # cfo and gender
            cfo_gender = data.groupby(
                  ['CFO_NEW', 'GENDER'])['EXEC_FULLNAME']
            calculate_cfo_gender = cfo_gender.count()
            
            print('''How many women and men have had the 
                  title CFO at least once''', 
                  calculate_cfo_gender)
            
            # average age women VS men
            av_age = data.groupby(
                  'GENDER', as_index=False)['AGE'].mean()
                                  
            print('The average age of women VS men is',
                  av_age)
            
            # average salary women VS men
            av_salary = data.groupby(
                  'GENDER', as_index = False)['SALARY'].mean()
                                     
            print('The average salary of women VS men', 
                  av_salary)
            
            # average salary percentange change women VS men
            av_pct_sal = data.groupby(
                  'GENDER', as_index = False)['SAL_PCT'].mean()
                                      
            print('The salary percentage change of women VS men', 
                  av_pct_sal)
            
            # average total compensation women VS men
            av_tdc1_gender = data.groupby(
                  'GENDER', as_index= False)['TDC1'].mean()
                                          
            av_tdc2_gender = data.groupby(
                  'GENDER', as_index = False)['TDC2'].mean()
                                          
            print('The total 1 year compensation by gender is:',
                  av_tdc1_gender)
            
            print('The total 2 year compensation by gender is:',
                  av_tdc2_gender)
            
            # bonus women VS men
            av_bonus_gender = data.groupby(
                  'GENDER', as_index = False)['BONUS'].mean()
                                           
            print('The average bonus men VS women', 
                  av_bonus_gender)
            
            # average rank women VS men
            av_rank_gender = data.groupby(
                  'GENDER', as_index= False)['EXECRANK']                                        
            av_rank_gender = av_rank_gender.apply(statistics.mode)
            
            print('The average compensation rank per gender is:',
                  av_rank_gender)
            
            # average annual rank women VS men
            av_anrank_gender = data.groupby(
                  'GENDER', as_index = False)['EXECRANKANN']                                           
            av_anrank_gender = av_anrank_gender.apply(statistics.mode)
            
            print('The annual average compensation rank by gender is',
                  av_anrank_gender)
            
            # reason leaving the company women VS men 
            reason_gender = data.groupby(
                  ['REASON', 'GENDER'])['EXEC_FULLNAME']                                     
            reason_gender = reason_gender.count()
            
            print('''The most common reason that men and 
                  women are leaving the company''', reason_gender)
                  
            
            # spindex and gender
            spindex_gender = data.groupby(
                  'GENDER', as_index = False)['SPINDEX']                               
            spindex_gender = spindex_gender.apply(statistics.mode)
            
            print('''The main kind of companies that women 
                  belong VS men (SPINDEX)''', spindex_gender)
                  
            
            # spcode and gender
            spcode_gender = data.groupby(
                  'GENDER', as_index = False)['SPCODE']                    
            spcode_gender = spcode_gender.apply(statistics.mode)
            
            print('''The main kind of firms that women belong 
                  VS men (SPCODE)''', spcode_gender)
                  
            return
      
      def ceo_gender_comparisons(data):
            '''
            The current function performs initial comparisons 
            to the data using a double filter which is the 
            PCEO and the gender variable.
            '''
            # average age women VS men PCEOs
            av_age_genderceo = data.groupby(
                  ['GENDER', 'PCEO'], as_index= False)['AGE'].mean()
                                            
            print('''The average age of men and women that have 
                  currently the title PCEO''', av_age_genderceo)
                  
            
            # average age women VS men PCFOs
            av_age_gendercfo = data.groupby(
                  ['GENDER', 'PCFO'], as_index = False)['AGE'].mean()
                                            
            print('''The average age of women and men that have 
                  currently the title PCFO:''', av_age_gendercfo)
                  
            
            # average age of ceos
            av_age_ceos_gender = data.groupby(
                  ['GENDER', 'CEO_NEW'], as_index= False)['AGE'].mean()
                                              
            print('''The average age of women VS men that have had 
                  at least once the title CEO''', av_age_ceos_gender)
                  
            
            # average age of cfos
            av_age_cfos_gender = data.groupby(
                  ['GENDER', 'CFO_NEW'],as_index = False)['AGE'].mean()
                                              
            print('''The average age of women VS men that have had 
                  at least once the title CFO''', av_age_cfos_gender)
                  
            
            # average salary woman VS men pceos
            av_salary_ceo_gender = data.groupby(
                  ['GENDER', 'PCEO'], as_index= False)['SALARY'].mean()
                                                
            print('''The average salary of women and men that have 
                  currently the title CEO''', av_salary_ceo_gender)
                  
            
            # average salary women VS men pcfos
            av_salary_cfo_gender = data.groupby(
                  ['GENDER','PCFO'], as_index= False)['SALARY'].mean() 
                                                
            print('''The average salary of women and men that have 
                  currently the title CFO''', av_salary_cfo_gender)
                  
            
            #average salary of ceos
            av_sal_ceos_gender = data.groupby(
                  ['GENDER', 'CEO_NEW'], as_index= False)['SALARY'].mean()
                                              
            print('''The average salary of women Vs men that have had 
                  at least once the title CEO''', av_sal_ceos_gender)
                  
            
            # average salary of cfos
            av_sal_cfos_gender = data.groupby(
                  ['GENDER', 'CFO_NEW'], as_index= False)['SALARY'].mean()
                                              
            print('''The average salary of women Vs men that have had at 
                  least once the title CFO''', av_sal_cfos_gender)
                  
            
            # average 1 year total compensation of women VS men current pceos
            av_tdc1_ceo_gender = data.groupby(
                  ['GENDER', 'PCEO'], as_index = False)['TDC1'].mean()
                                              
            print('''The average 1 year total compensation of women VS 
                  men that currently have the title CEO''',
              av_tdc1_ceo_gender)
            
            # average 1 year total compansation of women VS men current pcfos
            av_tdc1_cfo_gender = data.groupby(
                  ['GENDER', 'PCFO'], as_index = False)['TDC1'].mean()
                                              
            print('''The average 1 year total compensation of women VS men 
                  that currently have the title CFO''',
              av_tdc1_cfo_gender)
            
            # average total 1 year compensation of ceos
            av_tdc1_ceos_gender = data.groupby(
                  ['GENDER', 'CEO_NEW'], as_index= False)['TDC1'].mean()
                                               
            print('''The average total 1 year compensation of women VS
                   men that have had at least once the CEO title''',
                  av_tdc1_ceos_gender)
            
            # average total 1 year compensation of cfos
            av_tdc1_cfos_gender = data.groupby(
                  ['GENDER', 'CFO_NEW'], as_index = False)['TDC1'].mean()
                                                
            print('''The average total 1 year compensation of women VS men 
                  that have had at least once the title CFO''',
                  av_tdc1_cfos_gender)
            
            # average 2 year total compensation 
            # of women VS men current pceos
            av_tdc2_ceo_gender = data.groupby(
                  ['GENDER', 'PCEO'], as_index = False)['TDC2'].mean()
            
            print('''The average 2 year total compensation of women VS men 
                  that have currently the title CEO''',
              av_tdc2_ceo_gender)
            
            # average 2 year total compensation of 
            # women VS men current pcfos
            av_tdc2_cfo_gender = data.groupby(
                  ['GENDER', 'PCFO'], as_index = False)['TDC2'].mean()
            
            print('''The average 2 year total compensation of women VS men 
                  that have currently the title CFO''',
              av_tdc2_cfo_gender)
            
             # average total 2 year compensation of ceos
            av_tdc2_ceos_gender = data.groupby(
                  ['GENDER', 'CEO_NEW'], as_index = False)['TDC2'].mean()
            
            print('''The average total 2 year compensation of women VS men 
                  that have had at least once the title CEO''',
                  av_tdc2_ceos_gender)
            
            # average total 2 year compensation of cfos
            av_tdc2_cfos_gender = data.groupby(
                  ['GENDER','CFO_NEW'], as_index = False)['TDC2'].mean()
             
            print('''The average total 2 year compensation of women VS men 
                  that have had at least once the titls CFO''',
                  av_tdc2_cfos_gender)
            
            # bonus differences of women VS men current pceos
            av_bonus_pceo = data.groupby(
                  ['GENDER', 'PCEO'], as_index = False)['BONUS'].mean()
            
            print('The average bonus men VS women that are currently CEOs', 
                  av_bonus_pceo)
            
            # bonus differences of women VS men current pcfos
            av_bonus_pcfo = data.groupby(
                  ['GENDER', 'PCFO'], as_index = False)['BONUS'].mean()
            
            print('The average bonus men VS women that are currently CFOs', 
                  av_bonus_pcfo)
            
            # bonus differences of women VS men current ceos
            av_bonus_ceo = data.groupby(
                  ['GENDER', 'CEO_NEW'], as_index = False)['BONUS'].mean()
            
            print('''The average bonus differences of men VS women that were 
                  CEOs at least once''', av_bonus_ceo)
                  
            # bonus differences of women VS men current cfos
            av_bonus_cfo = data.groupby(
                  ['GENDER', 'CFO_NEW'], as_index = False)['BONUS'].mean()
            print('''The average bonus differences of men VS women that were 
                  CFOs at least once''', av_bonus_cfo)
                  
            return
            
        

      
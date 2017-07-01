import { TestBed, inject } from '@angular/core/testing';

import { InfoMessagesService } from './info-messages.service';

describe('InfoMessagesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [InfoMessagesService]
    });
  });

  it('should be created', inject([InfoMessagesService], (service: InfoMessagesService) => {
    expect(service).toBeTruthy();
  }));
});
